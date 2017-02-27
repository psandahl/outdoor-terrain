module DummyGen
    ( makeDummyMesh
    ) where

import           Graphics.LWGL              (BufferUsage (..), GLuint, Mesh)
import qualified Graphics.LWGL              as GL
import           Graphics.LWGL.Vertex_P_Tex (Vertex (..))
import           Linear                     (V2 (..), V3 (..))

makeDummyMesh :: IO Mesh
makeDummyMesh = GL.buildFromList StaticDraw vertices indices

vertices :: [Vertex]
vertices =
    [ Vertex
        { position = V3 (-1) 0 0
        , texCoord = V2 0 1
        }
    , Vertex
        { position = V3 0 0 0
        , texCoord = V2 1 1
        }
    , Vertex
        { position = V3 (-1) (-1) 0
        , texCoord = V2 0 0
        }
    , Vertex
        { position = V3 0 (-1) 0
        , texCoord = V2 1 0
        }
    ]

indices :: [GLuint]
indices = [0, 2, 1, 1, 2, 3]
