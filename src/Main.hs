module Main where

import           Control.Monad    (unless, when)
import           Data.Either      (isLeft)
import           Data.IORef       (IORef, modifyIORef, newIORef, readIORef,
                                   writeIORef)
import           Data.Maybe       (fromJust, isNothing)
import           Graphics.LWGL    (ClearBufferMask (..), EnableCapability (..),
                                   GLfloat, PolygonFace (..), PolygonMode (..))
import qualified Graphics.LWGL    as GL
import           Graphics.UI.GLFW (Key (..), KeyState (..), ModifierKeys,
                                   OpenGLProfile (..), StickyKeysInputMode (..),
                                   Window, WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           Linear
import           System.Exit      (exitFailure)

import           Camera           (Camera (matrix, position), Navigation (..),
                                   animate, initCamera, initNavigation)
import           EventLoop        (eventLoop)
import           SkyBox           (SkyBox, initSkyBox)
import qualified SkyBox
import           SunLight         (SunLight, initSun)
import           Terrain          (Terrain, initTerrain)
import qualified Terrain

data RenderState = RenderState
    { skyBox       :: !SkyBox
    , terrain      :: !Terrain
    , perspectiveM :: !(M44 GLfloat)
    , camera       :: !Camera
    , sunLight     :: !SunLight
    , navigation   :: !Navigation
    , lastTime     :: !Double
    } deriving Show

createGLContext :: IO Window
createGLContext = do
    initSuccess <- GLFW.init
    unless initSuccess $ do
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

    eSkyBox <- initSkyBox
    when (isLeft eSkyBox) $ do
        let Left err = eSkyBox
        putStrLn err
        GLFW.terminate
        exitFailure

    let Right skyBox' = eSkyBox

    eTerrain <- initTerrain
    when (isLeft eTerrain) $ do
        let Left err = eTerrain
        putStrLn err
        GLFW.terminate
        exitFailure

    let Right terrain' = eTerrain

    Just now <- GLFW.getTime

    let renderState =
          RenderState
            { skyBox = skyBox'
            , terrain = terrain'
            , perspectiveM = perspective (degToRad 45)
                                         ( fromIntegral width / fromIntegral height )
                                         0.001 10000
            , camera = initCamera (V3 0 15 0) 0
            , sunLight = initSun
            , navigation = initNavigation
            , lastTime = now
            }

    ref <- newIORef renderState

    GLFW.setKeyCallback window $ Just (keyCallback ref)

    GL.glClearColor 0 0 0.4 0
    GL.glEnable CullFace
    GL.glCullFace Back
    --GL.glPolygonMode FrontAndBack Line
    eventLoop window $ renderScene ref

    GLFW.terminate

renderScene :: IORef RenderState -> IO ()
renderScene ref = do
    renderState <- readIORef ref

    Just now <- GLFW.getTime

    let duration = now - lastTime renderState
        camera'  = animate (navigation renderState) duration (camera renderState)
        view     = matrix camera'

    -- The updated camera and the new timestamp must be written to the state.
    writeIORef ref renderState { camera = camera', lastTime = now }

    -- Clear frame buffers.
    GL.glClear [ColorBuffer, DepthBuffer]

    -- Render sky box.
    SkyBox.render (perspectiveM renderState) view
                  (position camera') (skyBox renderState)

    -- Render terrain.
    Terrain.render (perspectiveM renderState) view
                   (sunLight renderState) (terrain renderState)

keyCallback :: IORef RenderState -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback ref _ key _ keyState _ =
    case key of
        Key'Up    -> modifyIORef ref $ setForward (isActive keyState)
        Key'Down  -> modifyIORef ref $ setBackward (isActive keyState)
        Key'Left  -> modifyIORef ref $ setLeft (isActive keyState)
        Key'Right -> modifyIORef ref $ setRight (isActive keyState)
        Key'A     -> modifyIORef ref $ setUp (isActive keyState)
        Key'Z     -> modifyIORef ref $ setDown (isActive keyState)
        _         -> return ()

isActive :: KeyState -> Bool
isActive KeyState'Released = False
isActive _                 = True

setForward :: Bool -> RenderState -> RenderState
setForward val renderState =
    let nav  = navigation renderState
    in renderState { navigation = nav { forward = val } }

setBackward :: Bool -> RenderState -> RenderState
setBackward val renderState =
    let nav  = navigation renderState
    in renderState { navigation = nav { backward = val } }

setLeft :: Bool -> RenderState -> RenderState
setLeft val renderState =
    let nav  = navigation renderState
    in renderState { navigation = nav { left = val } }

setRight :: Bool -> RenderState -> RenderState
setRight val renderState =
    let nav  = navigation renderState
    in renderState { navigation = nav { right = val } }

setDown :: Bool -> RenderState -> RenderState
setDown val renderState =
    let nav = navigation renderState
    in renderState { navigation = nav { down = val } }

setUp :: Bool -> RenderState -> RenderState
setUp val renderState =
    let nav = navigation renderState
    in renderState { navigation = nav { up = val } }

width :: Int
width = 1024

height :: Int
height = 768

degToRad :: Float -> Float
degToRad deg = deg * (pi / 180)
