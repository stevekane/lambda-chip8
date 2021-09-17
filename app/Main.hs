{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16, Word32)
import Data.Array (Array, Ix, array, listArray, assocs, ixmap, (!), (//))
import System.Random (StdGen, mkStdGen, genWord8)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Texturing

import qualified Data.ByteString as BS
import qualified Graphics.UI.GLFW as GLFW

import Lib
import Rendering

type RAMAddress = Word16
type StackAddress = Word8
type RegisterAddress = Word8
type DisplayAddress = Word32
type Pixel = Bool
type InputState = Bool
type Registers = Array RegisterAddress Word8
type Rom = Array RAMAddress Word8
type Font = Array RAMAddress Word8
type Nibbles = (Word8, Word8, Word8, Word8)
type OpCode = (Word8, Word8, Word8, Word8, Word8, Word8, Word16)

data Chip8 = Chip8 {
  displayWidth  :: Word32,
  displayHeight :: Word32,
  randomSeed    :: StdGen,
  display       :: Array DisplayAddress Pixel,
  inputs        :: Array Word8 InputState,
  d             :: Word8,
  s             :: Word8,
  pc            :: RAMAddress,
  sp            :: StackAddress,
  i             :: RAMAddress,
  registers     :: Array RegisterAddress Word8,
  stack         :: Array StackAddress RAMAddress,
  ram           :: Array RAMAddress Word8
} deriving (Show)

callSubroutineAtNNN nnn cpu = cpu { 
  pc = nnn, 
  sp = sp cpu + 1,
  stack = stack cpu // [(sp cpu,pc cpu + 2)]
}

returnFromSubroutine cpu = cpu { 
  pc = stack cpu ! (sp cpu - 1),
  sp = sp cpu - 1
}

jumpToNNN nnn cpu = cpu { 
  pc = nnn 
}

jumpToV0PlusNNN v0 nnn cpu = cpu { 
  pc = word16 v0 + nnn 
}

skipIfVxIsNN vx nn cpu = cpu { 
  pc = pc cpu + if vx == nn then 4 else 2
}

skipUnlessVxIsNN vx nn cpu = cpu { 
  pc = pc cpu + if vx /= nn then 4 else 2
}

skipIfVxIsVy vx vy cpu = cpu { 
  pc = pc cpu + if vx == vy then 4 else 2
}

skipUnlessVxIsVy vx vy cpu = cpu { 
  pc = pc cpu + if vx /= vy then 4 else 2
}

setVxToNN x vx nn cpu = cpu { 
  pc = pc cpu + 2, 
  registers = registers cpu // [(x,nn)] 
}

setVxToVxPlusNN x vx nn cpu = cpu { 
  pc = pc cpu + 2, 
  registers = registers cpu // [(x,vx + nn)] 
}

setVxToVy x vx vy cpu = cpu { 
  pc = pc cpu + 2, 
  registers = registers cpu // [(x,vy)] 
}

setVxToVxOrVy x vx vy cpu = cpu { 
  pc = pc cpu + 2, 
  registers = registers cpu // [(x,vx .|. vy)] 
}

setVxToVxAndVy x vx vy cpu = cpu { 
  pc = pc cpu + 2, 
  registers = registers cpu // [(x,vx .&. vy)] 
}

setVxToVxXorVy x vx vy cpu = cpu { 
  pc = pc cpu + 2, 
  registers = registers cpu // [(x,vx `xor` vy)] 
}

setVxToVxPlusVy x vx vy cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x,sum), (0xF,toWord8 carry)]
} where (sum, carry) = vx `addWithCarry` vy

setVxToVxMinusVy x vx vy cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x,difference), (0xF,toWord8 (not borrow))]
} where (difference, borrow) = vx `subtractWithBorrow` vy

setVxToVyMinusVx x vx vy cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x,difference), (0xF,toWord8 (not borrow))]
} where (difference, borrow) = vy `subtractWithBorrow` vx

rightShiftVxAndStoreLSBVx x vx cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x,vx `shiftR` 1), (0xF,lsbvx)]
} where lsbvx = toWord8 (nthbit 0 vx)

leftShiftVxAndStoreMSBVx x vx cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x,vx `shiftL` 1), (0xF,msbvx)]
} where msbvx = toWord8 (nthbit 7 vx)

setVxToRandAndNN x nn cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x,randValue .&. nn)],
  randomSeed = randomSeed'
} where (randValue, randomSeed') = genWord8 (randomSeed cpu)

clearDisplay cpu = cpu {
  pc = pc cpu + 2,
  display = initialize (0, displayWidth cpu * displayHeight cpu - 1) False
}

drawSimple :: Word8 -> Word8 -> Word8 -> Chip8 -> Chip8
drawSimple vx vy n cpu = cpu {
  pc = pc cpu + 2,
  display = display cpu // pixels
} where
  (x0,y0) = (word32 vx, word32 vy)
  (w,h)   = (8,word32 n)
  offsets = [0..(w-1)] × [0..(h-1)]
  ramOffset = i cpu
  pixels = fmap writePixel offsets
  writePixel (i,j) = (index,pixel)
    where
      (x,y) = (x0 + i, y0 + j)
      index = y * displayWidth cpu + x
      pixel = nthbit (7 - i) (ram cpu ! (ramOffset + word16 j))

-- drawSpriteAtIToVxVyNHigh vx vy n cpu = cpu {  
--   pc = pc cpu + 2,
--   display = display cpu // pixels,
--   registers = registers cpu // [(0xF,toWord8 collisionFlag)]
-- } where 
--   (w,h)            = (8,word32 n)
--   (xRaw,yRaw)      = (word32 vx,word32 vy)
--   (xMin,yMin)      = wrap (displayWidth,displayHeight) (xRaw,yRaw)
--   (xMax,yMax)      = bound (displayWidth,displayHeight) (w,h) (xMin,yMin)
--   coordinates      = [0..(xMax - xMin)] × [0..(yMax - yMin)]
--   ramOffset        = i cpu
--   displayOffset    = to1DIndex displayWidth (xMin,yMin)
--   pixels           = fmap writePixel coordinates
--   collisionFlag    = foldr detectCollision False coordinates
--   pixelFromDisplay offset (x,y) cpu = display cpu ! to1DIndex (displayWidth cpu) (x,y)
--   writePixel (x,y) = (index,pixel)
--     where 
--       index        = displayOffset + to1DIndex displayWidth (x,y)
--       ramPixel     = pixelFromRam ramOffset (word16 x,word16 y) cpu
--       displayPixel = pixelFromDisplay displayOffset (x,y) cpu
--       pixel        = ramPixel `xor` displayPixel
--   detectCollision (x,y) cf = cf || collision
--     where
--       ramPixel     = pixelFromRam ramOffset (word16 x,word16 y) cpu
--       displayPixel = pixelFromDisplay displayOffset (x,y) cpu
--       collision    = ramPixel && displayPixel

setIToNNN nnn cpu = cpu {
  pc = pc cpu + 2,
  i = nnn
}

setIToIPlusVx vx cpu = cpu {
  pc = pc cpu + 2,
  i = i cpu + word16 vx
}

setIToISpriteAddressVx vx cpu = cpu {
  pc = pc cpu + 2,
  i = word16 vx * fontHeight
} where fontHeight = 5

storeBCDVxAtI vx cpu = cpu {
  pc = pc cpu + 2,
  ram = ram cpu // [(i cpu,hundreds), (i cpu + 1,tens), (i cpu + 2,ones)]
} where (hundreds, tens, ones) = digits vx

dumpRegistersV0ToVxToI x cpu = cpu {
  pc = pc cpu + 2,
  ram = copyTo (ram cpu,i cpu) (registers cpu,0) x
}

loadRegistersV0ToVxFromI x cpu = cpu {
  pc = pc cpu + 2,
  registers = copyTo (registers cpu,0) (ram cpu,i cpu) x
}

setVxToD x cpu = cpu {
  pc = pc cpu + 2,
  registers = registers cpu // [(x,d cpu)]
}

setDToVx vx cpu = cpu {
  pc = pc cpu + 2,
  d = vx
}

setSToVx vx cpu = cpu {
  pc = pc cpu + 2,
  s = vx
}

skipIfKeyDownVx vx cpu = cpu {
  pc = pc cpu + if inputs cpu ! vx then 4 else 2
}

skipUnlessKeyDownVx vx cpu = cpu {
  pc = pc cpu + if inputs cpu ! vx then 2 else 4
}

blockUnlessKeyDownVx vx cpu = cpu {
  pc = pc cpu + if inputs cpu ! vx then 2 else 0
}

seed :: StdGen -> Chip8
seed randomSeed = Chip8 {
  displayWidth = 64,
  displayHeight = 32,
  randomSeed = randomSeed,
  display = initialize (0,64 * 32 - 1) False,
  inputs = initialize (0,0xF) False,
  registers = initialize (0,0xF) 0, 
  stack = initialize (0,0xF) 0,
  ram = initialize (0,0xFFF) 0,
  pc = 0x200,
  sp = 0, 
  d = 0,
  s = 0,
  i = 0
}

loadFont :: Font -> Chip8 -> Chip8
loadFont font cpu = cpu {
  ram = ram cpu // assocs font
}

loadRom :: Rom -> Chip8 -> Chip8
loadRom rom cpu = cpu {
  pc = 0x200,
  ram = ram cpu // fmap (shiftBy 0x200) (assocs rom)
} where shiftBy o (i,j) = (i + o,j)

fetch :: Chip8 -> Nibbles
fetch cpu = (highNibble b0, lowNibble b0, highNibble b1, lowNibble b1)
  where 
    b0 = ram cpu ! pc cpu
    b1 = ram cpu ! (pc cpu + 1)

execute :: Nibbles -> Chip8 -> Chip8
execute (a,x,y,n) cpu = case (a,x,y,n) of 
  (0x2, _, _, _)       -> callSubroutineAtNNN nnn cpu
  (0x0, 0x0, 0xE, 0xE) -> returnFromSubroutine cpu
  (0x1, _, _, _)       -> jumpToNNN nnn cpu
  (0xB, _, _, _)       -> jumpToV0PlusNNN v0 nnn cpu
  (0x3, _, _, _)       -> skipIfVxIsNN vx nn cpu
  (0x4, _, _, _)       -> skipUnlessVxIsNN vx nn cpu
  (0x5, _, _, 0x0)     -> skipIfVxIsVy vx vy cpu
  (0x9, _, _, 0x0)     -> skipUnlessVxIsVy vx vy cpu
  (0x6, _, _, _)       -> setVxToNN x vx nn cpu
  (0x7, _, _, _)       -> setVxToVxPlusNN x vx nn cpu
  (0x8, _, _, 0x0)     -> setVxToVy x vx vy cpu
  (0x8, _, _, 0x1)     -> setVxToVxOrVy x vx vy cpu
  (0x8, _, _, 0x2)     -> setVxToVxAndVy x vx vy cpu
  (0x8, _, _, 0x3)     -> setVxToVxXorVy x vx vy cpu
  (0x8, _, _, 0x4)     -> setVxToVxPlusVy x vx vy cpu
  (0x8, _, _, 0x5)     -> setVxToVxMinusVy x vx vy cpu
  (0x8, _, _, 0x7)     -> setVxToVyMinusVx x vx vy cpu
  (0x8, _, _, 0x6)     -> rightShiftVxAndStoreLSBVx x vx cpu
  (0x8, _, _, 0xE)     -> leftShiftVxAndStoreMSBVx x vx cpu
  (0xC, _, _, _)       -> setVxToRandAndNN x nn cpu
  (0x0,0x0,0xE,0x0)    -> clearDisplay cpu
  -- (0xD, _, _, _)       -> drawSpriteAtIToVxVyNHigh vx vy n cpu
  (0xD, _, _, _)       -> drawSimple vx vy n cpu
  (0xA, _, _, _)       -> setIToNNN nnn cpu
  (0xF, _, 0x1, 0xE)   -> setIToIPlusVx vx cpu
  (0xF, _, 0x2, 0x9)   -> setIToISpriteAddressVx vx cpu
  (0xF, _, 0x3, 0x3)   -> storeBCDVxAtI vx cpu
  (0xF, _, 0x5, 0x5)   -> dumpRegistersV0ToVxToI (fromIntegral x) cpu
  (0xF, _, 0x6, 0x5)   -> loadRegistersV0ToVxFromI x cpu
  (0xF, _, 0x0, 0x7)   -> setVxToD x cpu
  (0xF, _, 0x1, 0x5)   -> setDToVx vx cpu
  (0xF, _, 0x1, 0x8)   -> setSToVx vx cpu
  (0xE, _, 0x9, 0xE)   -> skipIfKeyDownVx vx cpu
  (0xE, _, 0xA, 0x1)   -> skipUnlessKeyDownVx vx cpu
  (0xF, _, 0x0, 0xA)   -> blockUnlessKeyDownVx vx cpu
  _                    -> error $ "unknown opcode: " ++ show (a,x,y,n)
  where 
  vx = registers cpu ! x
  vy = registers cpu ! y
  v0 = registers cpu ! 0x0
  nnn = word16FromNibbles x y n
  nn = word8FromNibbles y n

onError :: GLFW.ErrorCallback
onError e s = putStrLn $ unwords [show e, show s]

onRefresh :: GLFW.WindowRefreshCallback
onRefresh e = putStrLn "refresh"

onResize :: GLFW.FramebufferSizeCallback
onResize e w h = do
  putStrLn $ "width:" ++ show w ++ " | height:" ++ show h
  let viewportPosition = Position 0 0
  let viewportSize = Size (fromIntegral w) (fromIntegral h)
  viewport $= (viewportPosition, viewportSize)

onKeyPressed :: GLFW.KeyCallback
onKeyPressed w key num state modifiers = putStrLn "keydown"

onShutdown :: GLFW.WindowCloseCallback
onShutdown e = putStrLn "shutdown"

updateLoop :: RenderContext -> Int -> Chip8 -> IO ()
updateLoop ctx count cpu = do
  let cpu' = runChip8 25 cpu
  let textureWidth = fromIntegral (displayWidth cpu)
  let textureHeight = fromIntegral (displayHeight cpu)
  let textureSize = TextureSize2D textureWidth textureHeight
  let textureFormats = (R8, Red, UnsignedByte)
  let textureData = foldr convertDisplayToWord8List [] (display cpu')
  let program = displayProgram ctx
  let uniforms = displayUniforms ctx 
  let textures = displayTextures ctx
  let writeableTexture = snd (head textures)
  uploadTexture2D writeableTexture textureSize textureFormats textureData

  let clearedColor = Color4 0 0 0 1
  clearColor $= clearedColor
  clear [ColorBuffer]

  let indexCount = 3
  render program indexCount (vao ctx) uniforms textures 
  GLFW.pollEvents
  GLFW.swapBuffers (window ctx)
  updateLoop ctx (count + 1) cpu'
  where
    convertDisplayToWord8List :: Bool -> [Word8] -> [Word8]
    convertDisplayToWord8List b e = saturateWord8 b : e
    runChip8 :: Int -> Chip8 -> Chip8
    runChip8 0 cpu = cpu
    runChip8 n cpu = runChip8 (n - 1) $ execute (fetch cpu) cpu { 
      s = max 0 (s cpu - 1),
      d = max 0 (d cpu - 1)
    }


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
  let width = displayWidth chip8 * windowScaleFactor
  let height = displayHeight chip8 * windowScaleFactor
  let viewportSize = Size (fromIntegral width) (fromIntegral height)
  let viewportPosition = Position 0 0
  let windowWidth = fromIntegral width
  let windowHeight = fromIntegral height
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

  updateLoop ctx 0 chip8