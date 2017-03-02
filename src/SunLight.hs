module SunLight
    ( SunLight (..)
    , initSun
    , render
    ) where

import           Graphics.LWGL          (BufferUsage (..),
                                         EnableCapability (..), GLfloat,
                                         Location, PrimitiveType (..), Program,
                                         ShaderType (..),
                                         VertexArrayObject (..))
import qualified Graphics.LWGL          as GL
import           Graphics.LWGL.Vertex_P (Vertex (..), makeVertexArrayObject)
import           Linear                 (M44, V3 (..), (!*!))

import           Helpers                (makeTranslate)

data SunLight = SunLight
    { program       :: !Program
    , mvpLocation   :: !Location
    , colorLocation :: !Location
    , vao           :: !VertexArrayObject
    , sunPosition   :: !(V3 GLfloat)
    , sunColor      :: !(V3 GLfloat)
    } deriving Show

initSun :: IO (Either String SunLight)
initSun = do
    eProgram <- GL.loadShaders [ (VertexShader, "shaders/sunlight.vert")
                               , (FragmentShader, "shaders/sunlight.frag")
                               ]
    case eProgram of
        Right prog -> do

            mvpLocation' <- GL.glGetUniformLocation prog "mvp"
            colorLocation' <- GL.glGetUniformLocation prog "sunColor"
            vao' <- makeVertexArrayObject $
                GL.setBufferFromList StaticDraw [Vertex {position = V3 0 0 0}]

            GL.glBindVertexArray (VertexArrayObject 0)

            return $ Right SunLight
                { program = prog
                , mvpLocation = mvpLocation'
                , colorLocation = colorLocation'
                , vao = vao'
                , sunPosition = V3 0 100 (-200)
                , sunColor = V3 (255 / 255) (255 / 255) (251 / 255)
                }

        Left err -> return $ Left err

render :: M44 GLfloat -> M44 GLfloat -> SunLight -> IO ()
render proj view sunLight = do
    GL.glEnable DepthTest

    GL.glUseProgram $ program sunLight
    GL.glBindVertexArray $ vao sunLight

    let mvp = proj !*! view !*! makeTranslate (V3 0 15 (-2))
    GL.setMatrix4 (mvpLocation sunLight) mvp
    GL.setVector3 (colorLocation sunLight) $ sunColor sunLight

    GL.glDrawArrays Points 0 1

    GL.glBindVertexArray (VertexArrayObject 0)
