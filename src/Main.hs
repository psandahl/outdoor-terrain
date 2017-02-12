module Main where

import           Control.Monad    (when)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           System.Exit      (exitFailure)

import           RenderLoop       (renderLoop)

import           Graphics.LWGL    (ClearBufferMask (..))
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

main :: IO ()
main = do
  window <- createGLContext 1024 768
  GLFW.makeContextCurrent (Just window)
  GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

  GL.glClearColor 0 0 0.4 0
  renderLoop window $ \_ ->
    GL.glClear [ColorBuffer]

  GLFW.terminate
