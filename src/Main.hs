module Main where

import           Control.Monad    (when)
import           Data.Either      (isLeft)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           Linear           (V2 (..), V3 (..))
import           System.Exit      (exitFailure)

import           Entity
import           Graphics.LWGL    (ClearBufferMask (..), glClear, glClearColor)
import           RenderLoop       (renderLoop)

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

    eEntity <- makeFloor
    when (isLeft eEntity) $ do
        let Left err = eEntity
        putStrLn err
        GLFW.terminate
        exitFailure

    let Right entity = eEntity

    glClearColor 0 0 0.4 0
    renderLoop window $ renderFrame entity

    GLFW.terminate

renderFrame :: Entity -> Window -> IO ()
renderFrame entity _ = do
    glClear [ColorBuffer]
    render entity
