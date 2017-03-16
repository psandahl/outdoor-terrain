module Main where

import           Control.Monad    (unless, when)
import           Data.Either      (isLeft)
import           Data.IORef       (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.LWGL    (ClearBufferMask (..), EnableCapability (..),
                                   PolygonFace (..), PolygonMode (..))
import qualified Graphics.LWGL    as GL
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   VideoMode (..), Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           Linear
import           System.Exit      (exitFailure)

import           Camera           (Camera (matrix, position), animate,
                                   initCamera, initNavigation)
import           EventLoop        (eventLoop)
import           Input            (initInput)
import           RenderState      (RenderState (..))
import qualified SkyBox
import qualified SunLight
import qualified Terrain

createGLContext :: Bool -> IO (Window, Int, Int)
createGLContext fullScreen = do
    initSuccess <- GLFW.init
    unless initSuccess $ do
        putStrLn "GLFW initialization failed"
        exitFailure

    GLFW.windowHint $ WindowHint'Resizable False
    GLFW.windowHint $ WindowHint'Samples 4
    GLFW.windowHint $ WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
    --GLFW.windowHint $ WindowHint'OpenGLDebugContext True
    GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core

    monitor <- if fullScreen then GLFW.getPrimaryMonitor
                             else return Nothing

    eWindow <- GLFW.createWindow defaultWidth defaultHeight "Outdoor terrain" monitor Nothing
    when (isNothing eWindow) $ do
        putStrLn "Failed to create GLFW window"
        GLFW.terminate
        exitFailure

    return (fromJust eWindow, defaultWidth, defaultHeight)

main :: IO ()
main = do
    (window, width, height) <- createGLContext False
    GLFW.makeContextCurrent (Just window)
    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    eSkyBox <- SkyBox.init
    when (isLeft eSkyBox) $ do
        let Left err = eSkyBox
        putStrLn err
        GLFW.terminate
        exitFailure

    let Right skyBox' = eSkyBox

    eTerrain <- Terrain.init
    when (isLeft eTerrain) $ do
        let Left err = eTerrain
        putStrLn err
        GLFW.terminate
        exitFailure

    let Right terrain' = eTerrain

    eSunLight <- SunLight.init
    when (isLeft eSunLight) $ do
        let Left err = eSunLight
        putStrLn err
        GLFW.terminate
        exitFailure
    let Right sunLight' = eSunLight

    let renderState =
          RenderState
            { skyBox = skyBox'
            , terrain = terrain'
            , perspectiveM = perspective (degToRad 45)
                                         ( fromIntegral width / fromIntegral height )
                                         0.001 10000
            , camera = initCamera (V3 0 15 0) 0
            , navigation = initNavigation
            , sunLight = sunLight'
            , timestamp = 0
            , frameDuration = 0
            , renderWireframe = False
            }

    ref <- newIORef renderState

    GL.glClearColor 0 0 0.4 0
    GL.glEnable CullFace
    GL.glCullFace Back
    GL.glEnable ProgramPointSize

    initInput window ref

    eventLoop window $ renderScene ref

    GLFW.terminate

renderScene :: IORef RenderState -> IO ()
renderScene ref = do
    renderState <- readIORef ref

    Just now <- GLFW.getTime

    let duration = now - timestamp renderState
        camera'  = animate (navigation renderState) duration (camera renderState)
        view     = matrix camera'

    -- The updated camera and the new timestamp must be written to the state.
    writeIORef ref renderState { camera = camera'
                               , timestamp = now
                               , frameDuration = duration
                               }

    -- Clear frame buffers.
    GL.glClear [ColorBuffer, DepthBuffer]

    -- Render sky box.
    SkyBox.render (perspectiveM renderState) view
                  (position camera') (skyBox renderState)

    -- Render sun light.
    SunLight.render (perspectiveM renderState) view (sunLight renderState)

    -- Render terrain.
    when (renderWireframe renderState) $
        GL.glPolygonMode FrontAndBack Line

    Terrain.render (perspectiveM renderState) view
                   (sunLight renderState) (position camera')
                   (terrain renderState)

    when (renderWireframe renderState) $
        GL.glPolygonMode FrontAndBack Fill

defaultWidth :: Int
defaultWidth = 1280

defaultHeight :: Int
defaultHeight = 960

degToRad :: Float -> Float
degToRad deg = deg * (pi / 180)
