module Entity
    ( Entity (..)
    --, EntityFactory
    --, initFactory
    , makeFloor
    , render
    ) where

import           Control.Monad              (forM_, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either
import           Data.Either
import           Linear                     (V2 (..), V3 (..))

import           Graphics.LWGL
import           Graphics.LWGL.Vertex_P_Tex

data Entity = Entity
    { program  :: !Program
    , mesh     :: !Mesh
    , animate  :: (Entity -> IO Entity)
    , textures :: ![(Texture, TextureTarget, Location)]
    }

--data EntityFactory = EntityFactory

--initFactory :: IO (Either String EntityFactory)
--initFactory = return $ Right EntityFactory

makeFloor :: IO (Either String Entity)
makeFloor = runEitherT $ do
    eProgram <- liftIO $
        loadShaders [(VertexShader, "shaders/lighted-box.vert")
                    , (FragmentShader, "shaders/lighted-box.frag")]
    when (isLeft eProgram) $ do
        let Left err = eProgram
        left err
    let Right prog = eProgram

    eTexture <- liftIO $ loadTexture2D "textures/stones.jpg" RGB8 False
    when (isLeft eTexture) $ do
        let Left err = eTexture
        left err
    let Right texture = eTexture

    mesh' <- liftIO $ buildFromList StaticDraw floorVertices
    texLoc <- liftIO $ glGetUniformLocation prog "tex1"

    right Entity
        { program = prog
        , mesh = mesh'
        , animate = \e -> return e
        , textures = [(texture, Texture2D, texLoc)]
        }


render :: Entity -> IO ()
render entity = do
    glUseProgram $ program entity
    glBindVertexArray (vao $ mesh entity)

    forM_ (zip (textures entity) [0 ..]) $ \((tex, targ, loc), unit) -> do
        glActiveTexture $ TextureUnit (fromIntegral unit)
        glBindTexture targ tex
        glUniform1i loc unit

    glDrawArrays Triangles 0 (numVertices $ mesh entity)

    glBindVertexArray (VertexArrayObject 0)

floorVertices :: [Vertex]
floorVertices =
    [ Vertex { position = V3   0.5    0.5  0, texCoord = V2 1 1 }
    , Vertex { position = V3 (-0.5)   0.5  0, texCoord = V2 0 1 }
    , Vertex { position = V3 (-0.5) (-0.5) 0, texCoord = V2 0 0 }
    , Vertex { position = V3   0.5    0.5  0, texCoord = V2 1 1 }
    , Vertex { position = V3 (-0.5) (-0.5) 0, texCoord = V2 0 0 }
    , Vertex { position = V3   0.5  (-0.5) 0, texCoord = V2 1 0 }
    ]
