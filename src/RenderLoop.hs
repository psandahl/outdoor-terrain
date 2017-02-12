module RenderLoop
    ( renderLoop
    ) where

import           Graphics.UI.GLFW (Key (..), KeyState (..), Window, getKey,
                                   pollEvents, swapBuffers, windowShouldClose)

-- | A simple rendering loop which repeats the provided action until ESC is
-- pressed or that a close event is created otherwise.
renderLoop :: Window -> (Window -> IO ()) -> IO ()
renderLoop window action = go
    where
        go :: IO ()
        go = do
            action window
            swapBuffers window
            pollEvents

            escState <- getKey window Key'Escape
            shouldClose <- windowShouldClose window
            if shouldClose || escState == KeyState'Pressed
                then return ()
                else go
