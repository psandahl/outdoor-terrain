module Terrain
    ( Terrain
    , initTerrain
    , render
    ) where

import           Graphics.LWGL (EnableCapability (..), GLfloat, Location,
                                Mesh (..), Program, ShaderType (..), Texture,
                                TextureFormat (..), TextureTarget (..),
                                TextureUnit (..), VertexArrayObject (..))
import qualified Graphics.LWGL as GL
import           Linear        (M44, V3 (..), (!*!))

import           Helpers       (makeTranslate)
import           SunLight      (SunLight (sunColor, sunPosition))
import           TerrainGen    (makeTerrainMeshFromMap)

data Terrain = Terrain
    { program       :: !Program
    , model         :: !(M44 GLfloat)
    , texture       :: !Texture
    , mvpLocation   :: !Location
    , modelLocation :: !Location
    , sunLocation   :: !Location
    , colorLocation :: !Location
    , eyeLocation   :: !Location
    , texLocation   :: !Location
    , mesh          :: !Mesh
    } deriving Show

initTerrain :: IO (Either String Terrain)
initTerrain = do
    eProgram <- GL.loadShaders [ (VertexShader, "shaders/terrain.vert")
                               , (FragmentShader, "shaders/terrain.frag")
                               ]
    case eProgram of

        Right prog -> do

            eTexture <- GL.loadTexture2D RGB8 True "textures/dirt01d.tga"
            case eTexture of
                Right texture' -> do

                    eMesh <- makeTerrainMeshFromMap "heightmaps/heightmap.bmp"
                                                    "colormaps/colormap.bmp"
                    --eMesh <- Right <$> makeDummyMesh
                    --eMesh <- Right <$> makeTerrainMesh 10 10 (constHeight 0)
                    case eMesh of

                        Right mesh' -> do
                            mvpLocation' <- GL.glGetUniformLocation prog "mvp"
                            modelLocation' <- GL.glGetUniformLocation prog "model"
                            sunLocation' <- GL.glGetUniformLocation prog "sunPosition"
                            colorLocation' <- GL.glGetUniformLocation prog "sunColor"
                            eyeLocation' <- GL.glGetUniformLocation prog "eyePosition"
                            texLocation' <- GL.glGetUniformLocation prog "groundTexture"

                            GL.glBindVertexArray (VertexArrayObject 0)

                            return $ Right Terrain
                                      { program = prog
                                      , model = makeTranslate $ V3 (-128.5) 0 (-128.5)
                                      , texture = texture'
                                      , mvpLocation = mvpLocation'
                                      , modelLocation = modelLocation'
                                      , sunLocation = sunLocation'
                                      , colorLocation = colorLocation'
                                      , eyeLocation = eyeLocation'
                                      , texLocation = texLocation'
                                      , mesh = mesh'
                                      }

                        Left err -> return $ Left err

                Left err -> return $ Left err

        Left err -> return $ Left err

render :: M44 GLfloat -> M44 GLfloat -> SunLight -> V3 GLfloat -> Terrain -> IO ()
render perspective view sunLight eyePosition terrain = do
    GL.glEnable DepthTest

    GL.glUseProgram $ program terrain

    let mvp = perspective !*! view !*! model terrain
    GL.setMatrix4 (mvpLocation terrain) mvp
    GL.setMatrix4 (modelLocation terrain) (model terrain)

    GL.setVector3 (sunLocation terrain) (sunPosition sunLight)
    GL.setVector3 (colorLocation terrain) (sunColor sunLight)
    GL.setVector3 (eyeLocation terrain) eyePosition

    GL.glBindVertexArray (vao $ mesh terrain)

    GL.glActiveTexture $ TextureUnit 0
    GL.glBindTexture Texture2D $ texture terrain
    GL.glUniform1i (texLocation terrain) 0

    GL.drawTrianglesVector (indices $ mesh terrain)

    GL.glBindVertexArray (VertexArrayObject 0)
