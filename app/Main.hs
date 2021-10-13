{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Prelude hiding (replicate)

import System.Random (mkStdGen)

import Control.Lens (Lens', set, over, view, (^.))

import Data.Word (Word8, Word16, Word32)
import Data.Vector (Vector(..), replicate, fromList, (//), (!))

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Texturing

import qualified Data.ByteString as BS
import qualified Graphics.UI.GLFW as GLFW

import Chip8 (Chip8(execute, display))
import Chip8Model (Chip8Model(..))
import Lib hiding (copyTo, (×))
import VectorUtils (indexPairs, fromByteString, copyTo)
import Rendering

-- System event handlers
onError :: GLFW.ErrorCallback
onError e s = putStrLn $ unwords [show e, show s]

onRefresh :: GLFW.WindowRefreshCallback
onRefresh e = putStrLn "refresh"

onResize :: GLFW.FramebufferSizeCallback
onResize e w h = do
  let viewportPosition = Position 0 0
  let viewportSize = Size (fromIntegral w) (fromIntegral h)
  viewport $= (viewportPosition, viewportSize)

onKeyPressed :: GLFW.KeyCallback
onKeyPressed w key num state modifiers = print key

onShutdown :: GLFW.WindowCloseCallback
onShutdown e = putStrLn "shutdown"

-- Core update loop for the application
updateLoop :: Chip8 c => RenderContext -> c -> IO()
updateLoop ctx cpu = do
  let textureWidth = 64
  let textureHeight = 32
  let textureSize = TextureSize2D textureWidth textureHeight
  let textureFormats = (R8, Red, UnsignedByte)
  let textureData = foldr (\b xs -> saturateWord8 b : xs) [] (view Chip8.display cpu)
  let program = displayProgram ctx
  let uniforms = displayUniforms ctx 
  let textures = displayTextures ctx
  let writeableTexture = snd (head textures)
  let indexCount = 3
  let clearedColor = Color4 0 0 0 1
  let instructionsPerCycle = 25

  uploadTexture2D writeableTexture textureSize textureFormats textureData
  clearColor $= clearedColor
  clear [ColorBuffer]
  render program indexCount (vao ctx) uniforms textures 
  GLFW.pollEvents
  GLFW.swapBuffers (window ctx)
  updateLoop ctx (ntimes instructionsPerCycle execute cpu)


main :: IO ()
main = do
  -- Emulator loading and initialization
  fontBinary <- BS.readFile "fonts/default-font.bin"
  ibmLogoBinary <- BS.readFile "roms/IBM-logo.bin"

  let rndSeed = mkStdGen 10
  let fontVector = fromByteString fontBinary
  let progVector = fromByteString ibmLogoBinary
  let ram = replicate 4096 0
  let withFont = fontVector `copyTo` (0,ram)
  let withProgram = progVector `copyTo` (512,withFont)

  let chip8 = Chip8Model {
    Chip8Model.i = 0,
    Chip8Model.pc = 0x200,
    Chip8Model.ram = withProgram,
    Chip8Model.stack = [],
    Chip8Model.registers = replicate 16 0,
    Chip8Model.display = replicate (64 * 32) False
  }

  -- Renderer loading and initialization
  let windowScaleFactor = 20
  let displayWidth = 64 * windowScaleFactor
  let displayHeight = 32 * windowScaleFactor
  let viewportSize = Size (fromIntegral displayWidth) (fromIntegral displayHeight)
  let viewportPosition = Position 0 0
  let windowWidth = fromIntegral displayWidth
  let windowHeight = fromIntegral displayHeight
  True <- GLFW.init
  Just window <- GLFW.createWindow windowWidth windowHeight "chip8" Nothing Nothing 
  viewport $= (viewportPosition,viewportSize)
  GLFW.defaultWindowHints
  GLFW.setErrorCallback (Just onError)
  GLFW.makeContextCurrent (Just window)
  GLFW.setWindowRefreshCallback window (Just onRefresh)
  GLFW.setFramebufferSizeCallback window (Just onResize)
  GLFW.setKeyCallback window (Just onKeyPressed)
  GLFW.setWindowCloseCallback window (Just onShutdown)

  -- setup geometry for full-screen triangle
  let v0 :: Vertex2 Float = Vertex2 (-4) (-4)
  let v1 :: Vertex2 Float = Vertex2 0  4
  let v2 :: Vertex2 Float = Vertex2 4 (-4)
  let vertices = [ v0, v1, v2 ]
  let size = fromIntegral (3 * 32)
  let numComponents = 2
  let dataType = Float
  vao <- mkVertexArrayObject size numComponents dataType vertices

  -- create texture for the display
  let textureUnit :: GLuint = 0
  let filter = (Nearest, Nearest)
  let wrapping = (Repeated, ClampToEdge)
  displayTexture <- mkTexture2D textureUnit filter wrapping

  -- Setup the shader program and its associated parameters
  vertexShaderCode <- readFile "shaders/screen-vertex.glsl"
  fragmentShaderCode <- readFile "shaders/screen-fragment.glsl"
  Compiled program <- mkShaderProgram vertexShaderCode fragmentShaderCode
  colorLocation <- uniformLocation program "color"
  bgLocation <- uniformLocation program "bgcolor"
  displayLocation <- uniformLocation program "display"
  let color = Color4Float (Color4 1 (165 / 255) 0 1)
  let bgcolor = Color4Float (Color4 (87 / 255) (102 / 255) (117 / 255) 1)
  let txUnit = TexUnit textureUnit
  let uniforms = [(colorLocation,color), (bgLocation,bgcolor), (displayLocation,txUnit)]
  let textures = [(textureUnit,displayTexture)]
  let ctx = RenderContext {
    window = window,
    vao = vao,
    displayProgram = program,
    displayUniforms = uniforms,
    displayTextures = textures 
  }

  updateLoop ctx chip8