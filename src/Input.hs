module Input
    ( initInput
    ) where

import           Control.Monad    (when)
import           Data.IORef       (IORef, modifyIORef)
import qualified Graphics.LWGL    as GL
import           Graphics.UI.GLFW (Key (..), KeyState (..), ModifierKeys,
                                   Window)
import qualified Graphics.UI.GLFW as GLFW

import           Camera           (Navigation (..))
import           Helpers          (makeProjection)
import           RenderState      (RenderState (..))

initInput :: Window -> IORef RenderState -> IO ()
initInput window ref = do
    GLFW.setKeyCallback window $ Just (keyCallback ref)
    GLFW.setWindowSizeCallback window $ Just (windowSizeCallback ref)

keyCallback :: IORef RenderState -> Window -> Key -> Int
            -> KeyState -> ModifierKeys -> IO ()
keyCallback ref _window key _scan keyState _modKeys = do

    -- Toggle the wireframe rendering mode.
    when (key == Key'R && keyState == KeyState'Pressed) $
        modifyIORef ref $ \state ->
            state { renderWireframe = not $ renderWireframe state }

    -- Camera turning left.
    when (key == Key'Left) $
        modifyIORef ref $ \state ->
            let nav = navigation state
            in state { navigation = nav { left = activeKey keyState } }

    -- Camera turning right.
    when (key == Key'Right) $
        modifyIORef ref $ \state ->
            let nav = navigation state
            in state { navigation = nav { right = activeKey keyState } }

    -- Camera go forward.
    when (key == Key'Up) $
        modifyIORef ref $ \state ->
            let nav = navigation state
            in state { navigation = nav { forward = activeKey keyState } }

    -- Camera go backward.
    when (key == Key'Down) $
        modifyIORef ref $ \state ->
            let nav = navigation state
            in state { navigation = nav { backward = activeKey keyState } }

    -- Camera go up.
    when (key == Key'A) $
        modifyIORef ref $ \state ->
            let nav = navigation state
            in state { navigation = nav { up = activeKey keyState } }

    -- Camera go down.
    when (key == Key'Z) $
        modifyIORef ref $ \state ->
            let nav = navigation state
            in state { navigation = nav { down = activeKey keyState } }

activeKey :: KeyState -> Bool
activeKey keyState =
    keyState == KeyState'Pressed || keyState == KeyState'Repeating

windowSizeCallback :: IORef RenderState -> Window -> Int -> Int -> IO ()
windowSizeCallback ref _window width height = do
    GL.glViewport 0 0 width height
    modifyIORef ref $ \state ->
        state { perspective = makeProjection width height}
