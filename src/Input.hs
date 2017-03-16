module Input
    ( initInput
    ) where

import           Control.Monad    (when)
import           Data.IORef       (IORef, modifyIORef)
import           Graphics.UI.GLFW (Key (..), KeyState (..), ModifierKeys,
                                   Window)
import qualified Graphics.UI.GLFW as GLFW

import           RenderState      (RenderState (..))

initInput :: Window -> IORef RenderState -> IO ()
initInput window ref = do
    GLFW.setKeyCallback window $ Just (keyCallback ref)

keyCallback :: IORef RenderState -> Window -> Key -> Int
            -> KeyState -> ModifierKeys -> IO ()
keyCallback ref _window key _scan keyState _modKeys = do

    -- Toggle the wireframe rendering mode.
    when (key == Key'R && keyState == KeyState'Pressed) $
        modifyIORef ref $ \state ->
            state { renderWireframe = not $ renderWireframe state }
