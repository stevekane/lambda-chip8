module Rendering where

import Data.Map (Map(..), empty, insert, lookup)
import Graphics.Rendering.OpenGL
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

data ShaderProgram
  = Compiled Program
  | Uncompiled String String String

data UniformSetting
  = Color4Float (Color4 GLfloat)
  | TexUnit GLuint

mkVertexArrayObject :: 
  Storable a => 
  GLsizeiptr ->  
  NumComponents ->
  DataType ->
  [a] -> 
  IO VertexArrayObject
mkVertexArrayObject size numComponents dataType vertices = do
  let location = AttribLocation 0
  let bufferOffset = plusPtr nullPtr 0
  let descriptor = VertexArrayDescriptor numComponents dataType 0 bufferOffset

  vao <- genObjectName
  vertexBuffer <- genObjectName 
  bindVertexArrayObject $= Just vao
  bindBuffer ArrayBuffer $= Just vertexBuffer
  withArray vertices $ \ptr -> do
    bufferData ArrayBuffer $= (size,ptr,StaticDraw)
  vertexAttribPointer location $= (ToFloat, descriptor)
  vertexAttribArray location $= Enabled
  bindVertexArrayObject $= Nothing
  return vao

mkShaderProgram :: 
  String -> 
  String -> 
  IO ShaderProgram
mkShaderProgram vertexShaderSrc fragmentShaderSrc = do
  vertexShader <- createShader VertexShader
  shaderSourceBS vertexShader $= packUtf8 vertexShaderSrc
  compileShader vertexShader
  vertexShaderCompiled <- get (compileStatus vertexShader)
  vertexShaderInfo <- get (shaderInfoLog vertexShader)

  fragmentShader <- createShader FragmentShader
  shaderSourceBS fragmentShader $= packUtf8 fragmentShaderSrc
  compileShader fragmentShader
  fragmentShaderCompiled <- get (compileStatus fragmentShader)
  fragmentShaderInfo <- get (shaderInfoLog fragmentShader)

  program <- createProgram
  attachShader program vertexShader
  attachShader program fragmentShader
  linkProgram program
  programLinked <- get (linkStatus program)
  programInfo <- get (programInfoLog program)

  if vertexShaderCompiled && fragmentShaderCompiled && programLinked
    then return (Compiled program)
    else return (Uncompiled vertexShaderInfo fragmentShaderInfo programInfo)

render :: 
  Program -> 
  NumArrayIndices -> 
  VertexArrayObject -> 
  [(UniformLocation,UniformSetting)] -> 
  [(GLuint,TextureObject)] ->
  IO()
render program count vao uniforms textures = do
  currentProgram $= Just program
  bindVertexArrayObject $= Just vao
  mapM_ setUniform uniforms
  mapM_ bindTexture2D textures
  drawArrays Triangles 0 count
  mapM_ unbindTexture2D textures
  bindVertexArrayObject $= Nothing
  currentProgram $= Nothing

setUniform :: (UniformLocation,UniformSetting) -> IO ()
setUniform (location, Color4Float c) = do 
  uniform location $= c
setUniform (location, TexUnit t) = do 
  uniform location $= t

bindTexture2D :: (GLuint,TextureObject) -> IO ()
bindTexture2D (unit,t) = do 
  activeTexture $= TextureUnit unit
  textureBinding Texture2D $= Just t

unbindTexture2D :: (GLuint,TextureObject) -> IO ()
unbindTexture2D (unit,_) = do 
  activeTexture $= TextureUnit unit
  textureBinding Texture2D $= Nothing