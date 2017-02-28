module Terrain
    ( Terrain
    , initTerrain
    , render
    ) where

import           Graphics.LWGL (EnableCapability (..), GLfloat, Location,
                                Mesh (..), PolygonFace (..), Program,
                                ShaderType (..), Texture, TextureFormat (..),
                                TextureTarget (..), TextureUnit (..),
                                VertexArrayObject (..))
import qualified Graphics.LWGL as GL
import           Linear        (M44, (!*!))

import           TerrainGen    (makeTerrainMeshFromMap)

data Terrain = Terrain
    { program     :: !Program
    , texture     :: !Texture
    , mvpLocation :: !Location
    , texLocation :: !Location
    , mesh        :: !Mesh
    } deriving Show

initTerrain :: IO (Either String Terrain)
initTerrain = do
    eProgram <- GL.loadShaders [ (VertexShader, "shaders/terrain.vert")
                               , (FragmentShader, "shaders/terrain.frag")
                               ]
    case eProgram of

        Right prog -> do

            eTexture <- GL.loadTexture2D RGB8 False "textures/dirt01d.tga"
            case eTexture of
                Right texture' -> do

                    eMesh <- makeTerrainMeshFromMap "heightmaps/heightmap.bmp"
                    --eMesh <- Right <$> makeDummyMesh
                    --eMesh <- Right <$> makeTerrainMesh 10 10 (constHeight 0)
                    case eMesh of

                        Right mesh' -> do
                            mvpLocation' <- GL.glGetUniformLocation prog "mvp"
                            texLocation' <- GL.glGetUniformLocation prog "groundTexture"
                            return $ Right Terrain
                                      { program = prog
                                      , texture = texture'
                                      , mvpLocation = mvpLocation'
                                      , texLocation = texLocation'
                                      , mesh = mesh'
                                      }

                        Left err -> return $ Left err

                Left err -> return $ Left err

        Left err -> return $ Left err

render :: M44 GLfloat -> M44 GLfloat -> Terrain -> IO ()
render perspective view terrain = do
    GL.glUseProgram $ program terrain
    GL.glEnable CullFace
    GL.glCullFace Back

    let mvp = perspective !*! view
    GL.setMatrix4 (mvpLocation terrain) mvp

    GL.glBindVertexArray (vao $ mesh terrain)

    GL.glActiveTexture $ TextureUnit 0
    GL.glBindTexture Texture2D $ texture terrain
    GL.glUniform1i (texLocation terrain) 0

    GL.drawTrianglesVector (indices $ mesh terrain)

    GL.glBindVertexArray (VertexArrayObject 0)
