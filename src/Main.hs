module Main where

import           Control.Monad              (when)
import           Data.Maybe                 (fromJust, isNothing)
import           Graphics.UI.GLFW           (OpenGLProfile (..),
                                             StickyKeysInputMode (..), Window,
                                             WindowHint (..))
import qualified Graphics.UI.GLFW           as GLFW
import           Linear                     (V2 (..), V3 (..))
import           System.Exit                (exitFailure)

import           RenderLoop                 (renderLoop)

import           Graphics.LWGL              (BufferTarget (..),
                                             BufferUsage (..),
                                             ClearBufferMask (..),
                                             ComponentCount (..), GLfloat,
                                             Location (..), PrimitiveType (..),
                                             Program, ShaderType (..), Texture,
                                             TextureFormat (..),
                                             VertexArrayObject (..),
                                             VertexAttribPointerType (..))
import qualified Graphics.LWGL              as GL
import           Graphics.LWGL.Vertex_P_Tex (Vertex (..), makeVertexArrayObject)

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

vertices :: [Vertex]
vertices =
    [ Vertex { position = V3   0.5    0.5  0, texCoord = V2 1 1 }
    , Vertex { position = V3 (-0.5)   0.5  0, texCoord = V2 0 1 }
    , Vertex { position = V3 (-0.5) (-0.5) 0, texCoord = V2 0 0 }
    , Vertex { position = V3   0.5    0.5  0, texCoord = V2 1 1 }
    , Vertex { position = V3 (-0.5) (-0.5) 0, texCoord = V2 0 0 }
    , Vertex { position = V3   0.5  (-0.5) 0, texCoord = V2 1 0 }
    ]

initGraphics :: IO VertexArrayObject
initGraphics = do
    vao <- makeVertexArrayObject StaticDraw vertices
    return vao

main :: IO ()
main = do
    window <- createGLContext 1024 768
    GLFW.makeContextCurrent (Just window)
    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    program <- loadShaders [ (VertexShader, "shaders/lighted-box.vert")
                           , (FragmentShader, "shaders/lighted-box.frag")]

    vao <- initGraphics
    texture <- loadTexture "textures/stones.jpg"

    GL.glClearColor 0 0 0.4 0
    renderLoop window $ \_ -> do
        GL.glClear [ColorBuffer]

        GL.glUseProgram program
        GL.glBindVertexArray vao
        GL.glDrawArrays Triangles 0 6

        GL.glBindVertexArray (VertexArrayObject 0)

    GLFW.terminate
