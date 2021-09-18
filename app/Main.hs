{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Random (mkStdGen)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Texturing

import qualified Data.ByteString as BS
import qualified Graphics.UI.GLFW as GLFW

import Chip8
import Rendering
import Array2D
import Lib

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

updateLoop :: RenderContext -> Chip8 -> IO ()
updateLoop ctx cpu = do
  let textureWidth = fromIntegral . width . display $ cpu
  let textureHeight = fromIntegral . height . display $ cpu
  let textureSize = TextureSize2D textureWidth textureHeight
  let textureFormats = (R8, Red, UnsignedByte)
  let textureData = fmap saturateWord8 (rowMajor . display $ cpu)
  let program = displayProgram ctx
  let uniforms = displayUniforms ctx 
  let textures = displayTextures ctx
  let writeableTexture = snd (head textures)
  let indexCount = 3
  let clearedColor = Color4 0 0 0 1

  uploadTexture2D writeableTexture textureSize textureFormats textureData
  clearColor $= clearedColor
  clear [ColorBuffer]
  render program indexCount (vao ctx) uniforms textures 
  GLFW.pollEvents
  GLFW.swapBuffers (window ctx)
  updateLoop ctx (runChip8 25 cpu)
  where
    runChip8 :: Int -> Chip8 -> Chip8
    runChip8 0 cpu = cpu
    runChip8 n cpu = runChip8 (n - 1) $ execute (fetch cpu) (decrementTimers cpu)

main :: IO ()
main = do
  -- Emulator loading and initialization
  fontBinary <- BS.readFile "fonts/default-font.bin"
  ibmLogoBinary <- BS.readFile "roms/IBM-logo.bin"
  testOpcodeBinary <- BS.readFile "roms/test-opcode.bin"
  trip8Binary <- BS.readFile "roms/trip-8-demo.bin"

  let rndSeed = mkStdGen 10
  let font = toArray fontBinary
  let rom = toArray trip8Binary
  let chip8 = loadRom rom $ loadFont font $ seed rndSeed

  -- Renderer loading and initialization
  let windowScaleFactor = 20
  let displayWidth = width (display chip8) * windowScaleFactor
  let displayHeight = height (display chip8) * windowScaleFactor
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