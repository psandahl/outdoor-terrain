module Main where

import           Control.Monad    (when)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           Linear
import           System.Exit      (exitFailure)

import           RenderLoop       (renderLoop)

import           Graphics.LWGL    (BufferTarget (..), BufferUsage (..),
                                   ClearBufferMask (..), ComponentCount (..),
                                   GLfloat, Location (..), PrimitiveType (..),
                                   Program, ShaderType (..), Texture,
                                   TextureFormat (..), VertexArrayObject (..),
                                   VertexAttribPointerType (..))
import qualified Graphics.LWGL    as GL

createGLContext :: Int -> Int -> IO Window
createGLContext width height = do
    initSuccess <- GLFW.init
    when (not initSuccess) $ do
        putStrLn "GLFW initialization failed"
        exitFailure

    GLFW.windowHint $ WindowHint'Samples 4
    GLFW.windowHint $ WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core

    window <- GLFW.createWindow width height "Outdoor terrain" Nothing Nothing
    when (isNothing window) $ do
        putStrLn "Failed to create GLFW window"
        GLFW.terminate
        exitFailure

    return $ fromJust window

loadShaders :: [(ShaderType, FilePath)] -> IO Program
loadShaders xs = do
    eShaders <- GL.loadShaders xs
    case eShaders of
        Right program -> return program
        Left err -> do
            putStrLn err
            GLFW.terminate
            exitFailure

loadTexture :: FilePath -> IO Texture
loadTexture file = do
    eTexture <- GL.loadTexture2D file RGB8 False
    case eTexture of
        Right texture -> return texture
        Left err -> do
            putStrLn err
            GLFW.terminate
            exitFailure

vertices :: [V3 GLfloat]
vertices =
    [ V3   0.5    0.5  0
    , V3 (-0.5)   0.5  0
    , V3 (-0.5) (-0.5) 0
    , V3   0.5    0.5  0
    , V3 (-0.5) (-0.5) 0
    , V3   0.5  (-0.5) 0
    ]

initGraphics :: IO VertexArrayObject
initGraphics = do
    [vao] <- GL.glGenVertexArray 1
    GL.glBindVertexArray vao

    texture <- loadTexture "textures/stones.jpg"

    [vbo] <- GL.glGenBuffers 1
    GL.glBindBuffer ArrayBuffer vbo
    GL.glBufferDataList ArrayBuffer vertices StaticDraw

    GL.glEnableVertexAttribArray (Location 0)
    GL.glVertexAttribPointer (Location 0) Three GLFloat False 0 0

    return vao

main :: IO ()
main = do
    window <- createGLContext 1024 768
    GLFW.makeContextCurrent (Just window)
    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    program <- loadShaders [ (VertexShader, "shaders/lighted-box.vert")
                           , (FragmentShader, "shaders/lighted-box.frag")]

    vao <- initGraphics

    GL.glClearColor 0 0 0.4 0
    renderLoop window $ \_ -> do
        GL.glClear [ColorBuffer]

        GL.glUseProgram program
        GL.glBindVertexArray vao
        GL.glDrawArrays Triangles 0 6

        GL.glBindVertexArray (VertexArrayObject 0)

    GLFW.terminate
