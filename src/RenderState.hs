module RenderState
    (RenderState (..)
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44)

import           Camera        (Camera, Navigation)
import           SkyBox        (SkyBox)
import           SunLight      (SunLight)
import           Terrain       (Terrain)

data RenderState = RenderState
    { skyBox          :: !SkyBox
    , terrain         :: !Terrain
    , perspectiveM    :: !(M44 GLfloat)
    , camera          :: !Camera
    , navigation      :: !Navigation
    , sunLight        :: !SunLight
    , timestamp       :: !Double
    , frameDuration   :: !Double
    , renderWireframe :: !Bool
} deriving Show
