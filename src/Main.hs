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
import           Linear           (V3 (..))
import           System.Exit      (exitFailure)

import           Camera           (Camera (matrix, position), animate,
                                   initCamera, initNavigation)
import           EventLoop        (eventLoop)
import           Helpers          (makeProjection)
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

    GLFW.windowHint $ WindowHint'Resizable True
    GLFW.windowHint $ WindowHint'Samples 4
    GLFW.windowHint $ WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
    --GLFW.windowHint $ WindowHint'OpenGLDebugContext True
    GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core

    window <- if fullScreen then makeFullscreen else makeWindow

    when (isNothing window) $ do
        putStrLn "Failed to create GLFW window"
        GLFW.terminate
        exitFailure

    return $ fromJust window

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
            , perspective = makeProjection width height
            , camera = initCamera (V3 0 15 0) 0
            , navigation = initNavigation
            , sunLight = sunLight'
            , timestamp = 0
            , frameDuration = 0
            , renderWireframe = False
            }

    ref <- newIORef renderState

    GL.glViewport 0 0 width height
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
    SkyBox.render (perspective renderState) view
                  (position camera') (skyBox renderState)

    -- Render sun light.
    SunLight.render (perspective renderState) view (sunLight renderState)

    -- Render terrain.
    when (renderWireframe renderState) $
        GL.glPolygonMode FrontAndBack Line

    Terrain.render (perspective renderState) view
                   (sunLight renderState) (position camera')
                   (terrain renderState)

    when (renderWireframe renderState) $
        GL.glPolygonMode FrontAndBack Fill

makeWindow :: IO (Maybe (Window, Int, Int))
makeWindow = do
    window <- GLFW.createWindow defaultWidth defaultHeight "Outdoor terrain" Nothing Nothing
    case window of
        Just window' -> return $ Just (window', defaultWidth, defaultHeight)
        Nothing      -> return Nothing

makeFullscreen :: IO (Maybe (Window, Int, Int))
makeFullscreen = do
    monitor <- GLFW.getPrimaryMonitor
    case monitor of
        Just monitor' -> do
            mode <- GLFW.getVideoMode monitor'
            case mode of
                Just mode' -> do
                    let w = videoModeWidth mode'
                        h = videoModeHeight mode'
                    window <- GLFW.createWindow w h "Outdoor terrain" (Just monitor') Nothing
                    case window of
                        Just window' -> return $ Just (window', w, h)
                        Nothing      -> return Nothing

                Nothing -> return Nothing

        Nothing -> return Nothing

defaultWidth :: Int
defaultWidth = 1024

defaultHeight :: Int
defaultHeight = 768
