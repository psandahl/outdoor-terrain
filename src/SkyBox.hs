module SkyBox
    ( SkyBox
    , init
    , render
    ) where

import           Graphics.LWGL          (BufferUsage (..),
                                         EnableCapability (..), GLfloat, GLuint,
                                         Location, Mesh (..), Program,
                                         ShaderType (..),
                                         VertexArrayObject (..))
import qualified Graphics.LWGL          as GL
import           Graphics.LWGL.Vertex_P (Vertex (..))
import           Linear                 (M44, V3 (..), (!*!))
import           Prelude                hiding (init)

import           Helpers                (makeTranslate)

data SkyBox = SkyBox
    { program     :: !Program
    , mvpLocation :: !Location
    , mesh        :: !Mesh
    } deriving Show

init :: IO (Either String SkyBox)
init = do
    eProgram <- GL.loadShaders [ (VertexShader, "shaders/skybox.vert")
                               , (FragmentShader, "shaders/skybox.frag")
                               ]
    case eProgram of
        Right prog -> do
            mvpLocation' <- GL.glGetUniformLocation prog "mvp"
            mesh' <- GL.buildFromList StaticDraw vertices indices'

            GL.glBindVertexArray (VertexArrayObject 0)

            return $ Right SkyBox
                { program = prog
                , mvpLocation = mvpLocation'
                , mesh = mesh'
                }

        Left err -> return $ Left err

render :: M44 GLfloat -> M44 GLfloat -> V3 GLfloat -> SkyBox -> IO ()
render proj view model skyBox = do
    -- The sky box must be rendered without depth information as it will
    -- be rendered close to the camera.
    GL.glDisable DepthTest

    GL.glUseProgram $ program skyBox

    GL.glBindVertexArray (vao $ mesh skyBox)

    let mvp = proj !*! view !*! makeTranslate model
    GL.setMatrix4 (mvpLocation skyBox) mvp

    GL.drawTrianglesVector (indices $ mesh skyBox)

    GL.glBindVertexArray (VertexArrayObject 0)

vertices :: [Vertex]
vertices =
    [ Vertex {position = V3 (-1) 1 (-1)}
    , Vertex {position = V3 1 1 (-1)}
    , Vertex {position = V3 (-1) 1 1}
    , Vertex {position = V3 1 1 1}

    , Vertex {position = V3 (-1) (-1) (-1)}
    , Vertex {position = V3 1 (-1) (-1)}
    , Vertex {position = V3 (-1) (-1) 1}
    , Vertex {position = V3 1 (-1) 1}
    ]

indices' :: [GLuint]
indices' =
    [ 0, 4, 1, 1, 4, 5 -- Front
    , 1, 5, 3, 3, 5, 7 -- Right
    , 2, 6, 0, 0, 6, 4 -- Left
    , 3, 7, 2, 2, 7, 6 -- Back
    , 2, 0, 3, 3, 0, 1 -- Top
    , 4, 6, 5, 5, 6, 7 -- Bottom
    ]
