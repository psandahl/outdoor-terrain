module Main where

import           Control.Monad    (when)
import           Data.Either      (isLeft)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           System.Exit      (exitFailure)

import           Graphics.LWGL    (ClearBufferMask (..), EnableCapability (..),
                                   glClear, glClearColor, glEnable)
import           RenderLoop       (renderLoop)
import           Terrain

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

main :: IO ()
main = do
    window <- createGLContext 1024 768
    GLFW.makeContextCurrent (Just window)
    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    eTerrain <- initTerrain
    when (isLeft eTerrain) $ do
        let Left err = eTerrain
        putStrLn err
        GLFW.terminate
        exitFailure

    let Right terrain0 = eTerrain
    terrain <- addPatch terrain0 <$> newPatch

    glClearColor 0 0 0.4 0
    glEnable DepthTest

    renderLoop window $ renderFrame terrain

    GLFW.terminate

renderFrame :: Terrain -> Window -> IO ()
renderFrame terrain _ = do
    glClear [ColorBuffer, DepthBuffer]
    render terrain
