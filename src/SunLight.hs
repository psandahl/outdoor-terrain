module SunLight
    ( SunLight (..)
    , initSun
    ) where

import           Graphics.LWGL          (BufferUsage (..), GLfloat,
                                         VertexArrayObject (..))
import qualified Graphics.LWGL          as GL
import           Graphics.LWGL.Vertex_P (Vertex (..), makeVertexArrayObject)
import           Linear                 (V3 (..))

data SunLight = SunLight
    { sunPosition :: !(V3 GLfloat)
    , sunColor    :: !(V3 GLfloat)
    , vao         :: !VertexArrayObject
    } deriving Show

initSun :: IO (Either String SunLight)
initSun = do
    vao' <- makeVertexArrayObject $ GL.setBufferFromList StaticDraw [Vertex {position = V3 0 0 0}]
    return $ Right SunLight
        { sunPosition = V3 0 100 (-200)
        , sunColor = V3 (255 / 255) (255 / 255) (251 / 255)
        , vao = vao'
        }
