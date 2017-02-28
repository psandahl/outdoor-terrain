module SunLight
    ( SunLight (..)
    , initSun
    ) where

import           Graphics.LWGL
import           Linear        (V3 (..))

data SunLight = SunLight
    { sunPosition :: !(V3 GLfloat)
    , sunColor    :: !(V3 GLfloat)
    } deriving Show

initSun :: SunLight
initSun =
    SunLight
        { sunPosition = V3 0 100 (-200)
        , sunColor = V3 (255 / 255) (255 / 255) (251 / 255)
        }
