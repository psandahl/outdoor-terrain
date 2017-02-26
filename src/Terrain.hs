module Terrain
    ( Terrain
    , initTerrain
    , render
    ) where

import           Graphics.LWGL          (BufferUsage (..),
                                         EnableCapability (..), GLfloat, GLuint,
                                         Location, Mesh (..), PolygonFace (..),
                                         Program, ShaderType (..),
                                         VertexArrayObject (..))
import qualified Graphics.LWGL          as GL
import           Graphics.LWGL.Vertex_P (Vertex (..))
import           Linear                 (M44, V3 (..), (!*!))

import           TerrainGen             (constHeight, makeTerrainMesh)

data Terrain = Terrain
    { program     :: !Program
    , mvpLocation :: !Location
    , mesh        :: !Mesh
    } deriving Show

initTerrain :: IO (Either String Terrain)
initTerrain = do
    eProgram <- GL.loadShaders [ (VertexShader, "shaders/terrain.vert")
                               , (FragmentShader, "shaders/terrain.frag")
                               ]
    case eProgram of
        Right prog -> do
            mvpLocation' <- GL.glGetUniformLocation prog "mvp"
            mesh' <- makeTerrainMesh 128 128 (constHeight 0)
            return $ Right Terrain
                        { program = prog
                        , mvpLocation = mvpLocation'
                        , mesh = mesh'
                        }

        Left err -> return $ Left err

render :: M44 GLfloat -> M44 GLfloat -> Terrain -> IO ()
render perspective view terrain = do
    GL.glUseProgram $ program terrain
    GL.glEnable CullFace
    GL.glCullFace Back

    let mvp = perspective !*! view
    GL.setMatrix4 (mvpLocation terrain) mvp

    GL.glBindVertexArray (vao $ mesh terrain)
    GL.drawTrianglesVector (indices $ mesh terrain)

    GL.glBindVertexArray (VertexArrayObject 0)
