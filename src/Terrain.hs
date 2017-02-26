module Terrain
    ( Terrain
    , Patch
    , initTerrain
    , newPatch
    , addPatch
    , render
    ) where

import           Control.Monad              (forM_)
import           Graphics.LWGL              (BufferUsage (..),
                                             EnableCapability (..), GLfloat,
                                             GLuint, Location, Mesh (..),
                                             PolygonFace (..), Program,
                                             ShaderType (..), Texture,
                                             TextureFormat (..),
                                             TextureTarget (..),
                                             TextureUnit (..),
                                             VertexArrayObject (..))
import qualified Graphics.LWGL              as GL
import           Graphics.LWGL.Vertex_P_Tex (Vertex (..))
import           Linear                     (M44, V2 (..), V3 (..), (!*!))

data Terrain = Terrain
    { program          :: !Program
    , textures         :: ![Texture]
    , textureLocations :: ![Location]
    , mvpLocation      :: !Location
    , patches          :: ![Patch]
    } deriving Show

data Patch = Patch
    { mesh :: !Mesh
    } deriving Show

initTerrain :: IO (Either String Terrain)
initTerrain = do
    eProgram <- GL.loadShaders [ (VertexShader, "shaders/terrain.vert")
                               , (FragmentShader, "shaders/terrain.frag")
                               ]
    case eProgram of
        Right prog -> do
            eTextures <-
                sequence <$> mapM (GL.loadTexture2D RGB8 False)
                    ["textures/grass.jpg"]
            case eTextures of
                Right ts -> do
                    textureLocations' <-
                        mapM (GL.glGetUniformLocation prog)
                            ["grassTexture"]

                    mvpLocation' <- GL.glGetUniformLocation prog "mvp"

                    return $ Right Terrain
                        { program = prog
                        , textures = ts
                        , textureLocations = textureLocations'
                        , mvpLocation = mvpLocation'
                        , patches = []
                        }
                Left err -> return $ Left err

        Left err -> return $ Left err

newPatch :: IO Patch
newPatch = Patch <$> GL.buildFromList StaticDraw vertices indices'

addPatch :: Terrain -> Patch -> Terrain
addPatch terrain patch =
    terrain { patches = patch : patches terrain }

render :: M44 GLfloat -> M44 GLfloat -> Terrain -> IO ()
render perspective view terrain = do
    GL.glUseProgram $ program terrain
    GL.glEnable CullFace
    GL.glCullFace Back

    let mvp = perspective !*! view
    GL.setMatrix4 (mvpLocation terrain) mvp

    forM_ (zip3 [0 ..]
                (textures terrain)
                (textureLocations terrain)) $ \(unit, texture, loc) -> do
        GL.glActiveTexture $ TextureUnit (fromIntegral unit)
        GL.glBindTexture Texture2D texture
        GL.glUniform1i loc unit

    forM_ (patches terrain) $ \patch -> do
        GL.glBindVertexArray (vao $ mesh patch)
        GL.drawTrianglesVector (indices $ mesh patch)

    GL.glBindVertexArray (VertexArrayObject 0)

vertices :: [Vertex]
vertices =
    [ Vertex { position = V3   0.5    0.5  0, texCoord = V2 1 1 }
    , Vertex { position = V3 (-0.5)   0.5  0, texCoord = V2 0 1 }
    , Vertex { position = V3 (-0.5) (-0.5) 0, texCoord = V2 0 0 }
    , Vertex { position = V3   0.5  (-0.5) 0, texCoord = V2 1 0 }
    ]

indices' :: [GLuint]
indices' =
    [0, 1, 2, 0, 2, 3]
