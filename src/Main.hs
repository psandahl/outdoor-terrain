module Main where

import           Control.Monad    (when)
import           Data.Either      (isLeft)
import           Data.IORef       (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.LWGL    (ClearBufferMask (..), EnableCapability (..),
                                   GLfloat, glClear, glClearColor, glEnable)
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           Linear
import           System.Exit      (exitFailure)

import           EventLoop        (eventLoop)
import           Terrain

data RenderState = RenderState
    { terrain      :: !Terrain
    , perspectiveM :: !(M44 GLfloat)
    , viewM        :: !(M44 GLfloat)
    } deriving Show

createGLContext :: IO Window
createGLContext = do
    initSuccess <- GLFW.init
    when (not initSuccess) $ do
        putStrLn "GLFW initialization failed"
        exitFailure

    GLFW.windowHint $ WindowHint'Samples 4
    GLFW.windowHint $ WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
    --GLFW.windowHint $ WindowHint'OpenGLDebugContext True
    GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core

    window <- GLFW.createWindow width height "Outdoor terrain" Nothing Nothing
    when (isNothing window) $ do
        putStrLn "Failed to create GLFW window"
        GLFW.terminate
        exitFailure

    return $ fromJust window

main :: IO ()
main = do
    window <- createGLContext
    GLFW.makeContextCurrent (Just window)
    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    eTerrain <- initTerrain
    when (isLeft eTerrain) $ do
        let Left err = eTerrain
        putStrLn err
        GLFW.terminate
        exitFailure

    let Right terrain0 = eTerrain
    terrain1 <- addPatch terrain0 <$> newPatch

    let renderState =
          RenderState
            { terrain = terrain1
            , perspectiveM = perspective (degToRad 45)
                                         ( fromIntegral width / fromIntegral height )
                                         0.001 10000
            , viewM = lookAt (V3 0 0 5) (V3 0 0 0) (V3 0 1 0)
            }

    ref <- newIORef renderState

    glClearColor 0 0 0.4 0
    glEnable DepthTest
    eventLoop window $ renderScene ref

    GLFW.terminate

renderScene :: IORef RenderState -> IO ()
renderScene ref = do
    renderState <- readIORef ref
    glClear [ColorBuffer, DepthBuffer]
    render (perspectiveM renderState) (viewM renderState) (terrain renderState)

width :: Int
width = 1024

height :: Int
height = 768

degToRad :: Float -> Float
degToRad deg = deg * (pi / 180)
