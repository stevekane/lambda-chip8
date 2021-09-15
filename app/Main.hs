{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forever)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor)
import Data.Word (Word8, Word16, Word32)
import Data.Array (Array, Ix, array, listArray, assocs, ixmap, (!), (//))
import System.Random (StdGen, mkStdGen, genWord8, uniformR)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Texturing
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import qualified Data.ByteString as BS
import qualified Graphics.UI.GLFW as GLFW

import Lib
import Rendering

-- Type aliases
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

-- Model for Chip8 CPU
data Chip8 = Chip8 {
  randomSeed :: StdGen,
  display    :: Array DisplayAddress Pixel,
  inputs     :: Array Word8 InputState,
  d          :: Word8,
  s          :: Word8,
  pc         :: RAMAddress,
  sp         :: StackAddress,
  i          :: RAMAddress,
  registers  :: Array RegisterAddress Word8,
  stack      :: Array StackAddress RAMAddress,
  ram        :: Array RAMAddress Word8
} deriving (Show)

-- Constants
instructionByteWidth = 2
displayWidth = 64
displayHeight = 32
off = False
on = True
blankDisplay = initialize (0,displayWidth * displayHeight - 1) off

-- Semantic helper functions
step pc = pc + instructionByteWidth

skipIf pc True = pc + instructionByteWidth + instructionByteWidth
skipIf pc False = pc + instructionByteWidth

blockIf pc True = pc
blockIf pc False = pc + instructionByteWidth

pixelFromRam offset (x,y) cpu = nthbit x (ram cpu ! (offset + y))

pixelFromDisplay offset (x,y) cpu = display cpu ! to1DIndex displayWidth (x,y)

-- OpCode transformations
callSubroutineAtNNN nnn cpu = cpu { 
  pc = nnn, 
  sp = sp cpu + 1,
  stack = stack cpu // [(sp cpu,pc cpu)]
}

returnFromSubroutine cpu = cpu { 
  pc = stack cpu ! sp cpu,
  sp = sp cpu - 1
}

jumpToNNN nnn cpu = cpu { 
  pc = nnn 
}

jumpToV0PlusNNN v0 nnn cpu = cpu { 
  pc = word16 v0 + nnn 
}

skipIfVxIsNN vx nn cpu = cpu { 
  pc = skipIf (pc cpu) (vx == nn) 
}

skipUnlessVxIsNN vx nn cpu = cpu { 
  pc = skipIf (pc cpu) (vx /= nn) 
}

skipIfVxIsVy vx vy cpu = cpu { 
  pc = skipIf (pc cpu) (vx == vy) 
}

skipUnlessVxIsVy vx vy cpu = cpu { 
  pc = skipIf (pc cpu) (vx /= vy) 
}

setVxToNN x vx nn cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,nn)] 
}

setVxToVxPlusNN x vx nn cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vx + nn)] 
}

setVxToVy x vx vy cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vy)] 
}

setVxToVxOrVy x vx vy cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vx .|. vy)] 
}

setVxToVxAndVy x vx vy cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vx .&. vy)] 
}

setVxToVxXorVy x vx vy cpu = cpu { 
  pc = step (pc cpu), 
  registers = registers cpu // [(x,vx `xor` vy)] 
}

setVxToVxPlusVy x vx vy cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,sum), (0xF,toWord8 carry)]
} where (sum, carry) = vx `addWithCarry` vy

setVxToVxMinusVy x vx vy cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,difference), (0xF,toWord8 (not borrow))]
} where (difference, borrow) = vx `subtractWithBorrow` vy

setVxToVyMinusVx x vx vy cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,difference), (0xF,toWord8 (not borrow))]
} where (difference, borrow) = vy `subtractWithBorrow` vx

rightShiftVxAndStoreLSBVx x vx cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,vx `shiftR` 1), (0xF,lsbvx)]
} where lsbvx = toWord8 (nthbit 0 vx)

leftShiftVxAndStoreMSBVx x vx cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,vx `shiftL` 1), (0xF,msbvx)]
} where msbvx = toWord8 (nthbit 7 vx)

setVxToRandAndNN x nn cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,randValue .&. nn)],
  randomSeed = randomSeed'
} where (randValue, randomSeed') = genWord8 (randomSeed cpu)

clearDisplay cpu = cpu {
  pc = step (pc cpu),
  display = blankDisplay
}

drawSpriteAtIToVxVyNHigh vx vy n cpu = cpu {  
  pc = step (pc cpu),
  display = display cpu // pixels,
  registers = registers cpu // [(0xF,toWord8 collisionFlag)]
} where 
  (w,h)            = (8,word32 n)
  (xRaw,yRaw)      = (word32 vx,word32 vy)
  (xMin,yMin)      = wrap (displayWidth,displayHeight) (xRaw,yRaw)
  (xMax,yMax)      = bound (displayWidth,displayHeight) (w,h) (xMin,yMin)
  coordinates      = [0..(xMax - xMin)] × [0..(yMax - yMin)]
  ramOffset        = i cpu
  displayOffset    = to1DIndex displayWidth (xMin,yMin)
  pixels           = fmap writePixel coordinates
  collisionFlag    = foldr detectCollision False coordinates
  writePixel (x,y) = (index,pixel)
    where 
      index        = displayOffset + to1DIndex displayWidth (x,y)
      ramPixel     = pixelFromRam ramOffset (word16 x,word16 y) cpu
      displayPixel = pixelFromDisplay displayOffset (x,y) cpu
      pixel        = ramPixel `xor` displayPixel
  detectCollision (x,y) cf = cf || collision
    where
      ramPixel     = pixelFromRam ramOffset (word16 x,word16 y) cpu
      displayPixel = pixelFromDisplay displayOffset (x,y) cpu
      collision    = ramPixel && displayPixel

setIToNNN nnn cpu = cpu {
  pc = step (pc cpu),
  i = nnn
}

setIToIPlusVx vx cpu = cpu {
  pc = step (pc cpu),
  i = i cpu + word16 vx
}

setIToISpriteAddressVx vx cpu = cpu {
  pc = step (pc cpu),
  i = word16 vx * fontHeight
} where fontHeight = 5

storeBCDVxAtI vx cpu = cpu {
  pc = step (pc cpu),
  ram = ram cpu // [(i cpu,hundreds), (i cpu + 1,tens), (i cpu + 2,ones)]
} where (hundreds, tens, ones) = digits vx

dumpRegistersV0ToVxToI x cpu = cpu {
  pc = step (pc cpu),
  ram = copyTo (ram cpu,i cpu) (registers cpu,0) x
}

loadRegistersV0ToVxFromI x cpu = cpu {
  pc = step (pc cpu),
  registers = copyTo (registers cpu,0) (ram cpu,i cpu) x
}

setVxToD x cpu = cpu {
  pc = step (pc cpu),
  registers = registers cpu // [(x,d cpu)]
}

setDToVx vx cpu = cpu {
  pc = step (pc cpu),
  d = vx
}

setSToVx vx cpu = cpu {
  pc = step (pc cpu),
  s = vx
}

skipIfKeyDownVx vx cpu = cpu {
  pc = skipIf (pc cpu) (inputs cpu ! vx) 
}

skipUnlessKeyDownVx vx cpu = cpu {
  pc = skipIf (pc cpu) (not (inputs cpu ! vx))
}

blockUnlessKeyDownVx vx cpu = cpu {
  pc = blockIf (pc cpu) (not (inputs cpu ! vx))
}

seed :: StdGen -> Chip8
seed randomSeed = Chip8 {
  randomSeed = randomSeed,
  display = blankDisplay,
  inputs = initialize (0,0xF) off,
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
  (0xD, _, _, _)       -> drawSpriteAtIToVxVyNHigh vx vy n cpu
  (0xA, _, _, _)       -> setIToNNN nnn cpu
  (0xF, _, 0x1, 0xE)   -> setIToIPlusVx vx cpu
  (0xF, _, 0x2, 0x9)   -> setIToISpriteAddressVx vx cpu
  (0xF, _, 0x3, 0x3)   -> storeBCDVxAtI vx cpu
  (0xF, _, 0x5, 0x5)   -> dumpRegistersV0ToVxToI x cpu
  (0xF, _, 0x6, 0x5)   -> loadRegistersV0ToVxFromI x cpu
  (0xF, _, 0x0, 0x7)   -> setVxToD x cpu
  (0xF, _, 0x1, 0x5)   -> setDToVx vx cpu
  (0xF, _, 0x1, 0x8)   -> setSToVx vx cpu
  (0xE, _, 0x9, 0xE)   -> skipIfKeyDownVx vx cpu
  (0xE, _, 0xA, 0x1)   -> skipUnlessKeyDownVx vx cpu
  (0xF, _, 0x0, 0xA)   -> blockUnlessKeyDownVx vx cpu
  _                    -> cpu
  where 
  x = x
  vx = registers cpu ! x
  vy = registers cpu ! y
  v0 = registers cpu ! 0x0
  nnn = word16FromNibbles x y n
  nn = word8FromNibbles y n

-- TODO: do i need this? need to define all callbacks for GLFW
printError e s = putStrLn $ unwords [show e, show s]

main :: IO ()
main = do
  -- Emulator loading and initialization
  fontBinary <- BS.readFile "fonts/default-font.bin"
  ibmLogoBinary <- BS.readFile "roms/IBM-logo.bin"

  let rndSeed = mkStdGen 10
  let font = toArray fontBinary
  let rom = toArray ibmLogoBinary
  let chip8 = loadRom rom $ loadFont font $ seed rndSeed

  -- Renderer loading and initialization
  let windowScaleFactor = 40
  let width = fromIntegral (displayWidth * windowScaleFactor)
  let height = fromIntegral (displayHeight * windowScaleFactor)
  True <- GLFW.init
  Just window <- GLFW.createWindow width height "chip8" Nothing Nothing 
  GLFW.defaultWindowHints
  GLFW.setErrorCallback (Just printError)
  GLFW.makeContextCurrent (Just window)
  -- GLFW.setWindowRefreshCallback window (Just onRefresh)
  -- GLFW.setFramebufferSizeCallback window (Just onResize)
  -- GLFW.setKeyCallback window (Just onKeyPressed)
  -- GLFW.setWindowCloseCallback window (Just onShutown)

  -- Store attribute locations
  -- TODO: maybe read this data from a file?
  let fullScreenTriangle :: [Vertex2 Float] = [ Vertex2 (-4) (-4), Vertex2 0 4, Vertex2 4 (-4) ]
  let size = fromIntegral (length fullScreenTriangle * sizeOf (head fullScreenTriangle))

  -- create vertex buffer, bind it, and fill with data
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles
  vertexBuffer <- genObjectName 
  bindBuffer ArrayBuffer $= Just vertexBuffer
  withArray fullScreenTriangle $ \ptr -> do
    bufferData ArrayBuffer $= (size,ptr,StaticDraw)

  let positionLocation = AttribLocation 0
  let bufferOffset = plusPtr nullPtr 0
  let positionDescriptor = VertexArrayDescriptor 2 Float 0 bufferOffset

  vertexAttribPointer positionLocation $= (ToFloat, positionDescriptor)
  vertexAttribArray positionLocation $= Enabled
  bindVertexArrayObject $= Nothing

  -- create texture, bind it, and set essential properties
  let textureUnit :: GLuint = 0
  let size = TextureSize2D (fromIntegral displayWidth) (fromIntegral displayHeight)
  let filter = ((Nearest, Nothing), Nearest)
  let wrap = (Repeated, ClampToEdge)

  displayTexture <- genObjectName
  texture Texture2D $= Enabled                      
  activeTexture $= TextureUnit textureUnit
  textureBinding Texture2D $= Just displayTexture
  textureFilter Texture2D $= filter
  textureWrapMode Texture2D S $= wrap
  textureWrapMode Texture2D T $= wrap
  textureBinding Texture2D $= Nothing

  -- Setup a program
  vertexShaderCode <- readFile "shaders/screen-vertex.glsl"
  fragmentShaderCode <- readFile "shaders/screen-fragment.glsl"
  Compiled program <- mkShaderProgram vertexShaderCode fragmentShaderCode
  colorLocation <- uniformLocation program "color"
  backgroundColorLocation <- uniformLocation program "backgroundColor"
  displayLocation <- uniformLocation program "display"
  let color = Color4 1 (195 / 255) (160 / 255) 1
  let backgroundColor = Color4 (132 / 255) (132 / 255) (99 / 255) 1
  let uniforms = [(colorLocation, Color4Float color),
                  (backgroundColorLocation, Color4Float backgroundColor),
                  (displayLocation, TexUnit textureUnit)]
  let textures = [(textureUnit,displayTexture)]

  forever $ do
    -- TODO: TEST DATA ONLY!!!!!!!!!!!!!!!!!
    let ram = display chip8 // [(0,True),(displayWidth * displayHeight - 1,True)]
    let textureData :: [Word8] = foldr (\b e -> (if b then 0xff else 0x00) : e) [] ram
    let viewportPosition = Position 0 0
    let viewportSize = Size (fromIntegral width) (fromIntegral height)

    GLFW.pollEvents 

    -- BEGIN - update texture data routine
    activeTexture $= TextureUnit textureUnit
    textureBinding Texture2D $= Just displayTexture
    withArray textureData $ \ptr -> do
      let pixelData = PixelData Red UnsignedByte ptr
      texImage2D Texture2D NoProxy 0 R8 size 0 pixelData
    textureBinding Texture2D $= Nothing
    -- END -- update texture routine

    viewport $= (viewportPosition, viewportSize)
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer]
    render program 3 triangles uniforms textures
    GLFW.swapBuffers window