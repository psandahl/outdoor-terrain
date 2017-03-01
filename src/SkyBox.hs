module SkyBox
    ( SkyBox
    , initSkyBox
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

import           Helpers                (makeTranslate)

data SkyBox = SkyBox
    { program     :: !Program
    , mvpLocation :: !Location
    , mesh        :: !Mesh
    } deriving Show

initSkyBox :: IO (Either String SkyBox)
initSkyBox = do
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
    GL.glEnable DepthTest

    GL.glUseProgram $ program skyBox

    GL.glBindVertexArray (vao $ mesh skyBox)

    let mvp = proj !*! view !*! makeTranslate model
    GL.setMatrix4 (mvpLocation skyBox) mvp

    GL.drawTrianglesVector (indices $ mesh skyBox)

    GL.glBindVertexArray (VertexArrayObject 0)

vertices :: [Vertex]
vertices =
    [ Vertex {position = V3 (-0.5) 0.5 (-0.5)}
    , Vertex {position = V3 0.5 0.5 (-0.5)}
    , Vertex {position = V3 (-0.5) (-0.5) (-0.5)}
    , Vertex {position = V3 0.5 (-0.5) (-0.5)}
    ]

indices' :: [GLuint]
indices' = [0, 2, 1, 1, 2, 3]
