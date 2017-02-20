module Terrain
    ( Terrain
    , Patch
    , initTerrain
    , newPatch
    , addPatch
    , render
    ) where

import           Control.Monad              (forM_)
import           Graphics.LWGL
import           Graphics.LWGL.Vertex_P_Tex (Vertex (..))
import           Linear                     (V2 (..), V3 (..))

data Terrain = Terrain
    { program          :: !Program
    , textures         :: ![Texture]
    , textureLocations :: ![Location]
    , patches          :: ![Patch]
    }

data Patch = Patch
    { mesh :: !Mesh
    }

initTerrain :: IO (Either String Terrain)
initTerrain = do
    eProgram <- loadShaders [ (VertexShader, "shaders/terrain.vert")
                            , (FragmentShader, "shaders/terrain.frag")
                            ]
    case eProgram of
        Right prog -> do
            eTextures <-
                sequence <$> mapM (loadTexture2D RGB8 False)
                    ["textures/grass.jpg"]
            case eTextures of
                Right ts -> do
                    locations <-
                        mapM (glGetUniformLocation prog)
                            ["grassTexture"]

                    return $ Right Terrain
                        { program = prog
                        , textures = ts
                        , textureLocations = locations
                        , patches = []
                        }
                Left err -> return $ Left err

        Left err -> return $ Left err

newPatch :: IO Patch
newPatch = Patch <$> buildFromList StaticDraw vertices

addPatch :: Terrain -> Patch -> Terrain
addPatch terrain patch =
    terrain { patches = patch : patches terrain }

render :: Terrain -> IO ()
render terrain = do
    glUseProgram $ program terrain

    forM_ (zip3 [0 ..]
                (textures terrain)
                (textureLocations terrain)) $ \(unit, texture, loc) -> do
        glActiveTexture $ TextureUnit (fromIntegral unit)
        glBindTexture Texture2D texture
        glUniform1i loc unit

    forM_ (patches terrain) $ \patch -> do
        glBindVertexArray (vao $ mesh patch)
        glDrawArrays Triangles 0 (numVertices $ mesh patch)

    glBindVertexArray (VertexArrayObject 0)

vertices :: [Vertex]
vertices =
    [ Vertex { position = V3   0.5    0.5  0, texCoord = V2 1 1 }
    , Vertex { position = V3 (-0.5)   0.5  0, texCoord = V2 0 1 }
    , Vertex { position = V3 (-0.5) (-0.5) 0, texCoord = V2 0 0 }
    , Vertex { position = V3   0.5    0.5  0, texCoord = V2 1 1 }
    , Vertex { position = V3 (-0.5) (-0.5) 0, texCoord = V2 0 0 }
    , Vertex { position = V3   0.5  (-0.5) 0, texCoord = V2 1 0 }
    ]
