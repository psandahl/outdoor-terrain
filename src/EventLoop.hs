module EventLoop
    ( eventLoop
    ) where

import           Control.Monad    (unless)
import           Graphics.UI.GLFW (Key (..), KeyState (..), Window, getKey,
                                   pollEvents, swapBuffers, windowShouldClose)

-- | A simple rendering event loop which repeats the provided action until ESC is
-- pressed or that a close event is created otherwise.
eventLoop :: Window -> IO () -> IO ()
eventLoop window action = go
    where
        go :: IO ()
        go = do
            pollEvents
            action
            swapBuffers window

            escState <- getKey window Key'Escape
            shouldClose <- windowShouldClose window
            unless (shouldClose || escState == KeyState'Pressed)
                go
