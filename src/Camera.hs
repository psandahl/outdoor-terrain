module Camera
    ( Camera (..)
    , Navigation (..)
    , initCamera
    , initNavigation
    , animate
    ) where

import           Linear

import           Graphics.LWGL

data Camera = Camera
    { yRotation :: !GLfloat
    , position  :: !(V3 GLfloat)
    , direction :: !(V3 GLfloat)
    , matrix    :: !(M44 GLfloat)
    }
    deriving Show

data Navigation = Navigation
    { forward  :: !Bool
    , backward :: !Bool
    , left     :: !Bool
    , right    :: !Bool
    , up       :: !Bool
    , down     :: !Bool
    } deriving Show

initCamera :: V3 GLfloat -> GLfloat -> Camera
initCamera pos rot =
    let cam = Camera { yRotation = rot
                     , position = pos
                     , direction = makeDirection rot
                     , matrix = identity
                     }
        mat = makeViewMatrix cam
    in cam { matrix = mat }

initNavigation :: Navigation
initNavigation =
    Navigation
        { forward = False
        , backward = False
        , left = False
        , right = False
        , up = False
        , down = False
        }

animate :: Navigation -> Double -> Camera -> Camera
animate navigation durationD camera =
    let duration = realToFrac durationD
        theta = duration * rotateSpeed * fullCircle
        animation = goBackward duration    $
                        goForward duration $
                        goUp duration      $
                        goDown duration    $
                        turnRight theta    $
                        turnLeft theta camera
    in animation { matrix = makeViewMatrix animation }
    where
        turnLeft :: GLfloat -> Camera -> Camera
        turnLeft theta cam =
            if left navigation
                then
                    let yRotation' = yRotation cam + theta
                    in cam { yRotation = yRotation'
                           , direction = makeDirection yRotation'
                           }
                else cam

        turnRight :: GLfloat -> Camera -> Camera
        turnRight theta cam =
            if right navigation
                then
                    let yRotation' = yRotation cam - theta
                    in cam { yRotation = yRotation'
                           , direction = makeDirection yRotation'
                           }
                else cam

        goDown :: GLfloat -> Camera -> Camera
        goDown duration cam =
            if up navigation
                then
                    let stride = duration * moveSpeed *^ V3 0 1 0
                    in cam { position = position cam + stride }
                else cam

        goUp :: GLfloat -> Camera -> Camera
        goUp duration cam =
            if down navigation
                then
                    let stride = duration * moveSpeed *^ V3 0 1 0
                    in cam { position = position cam - stride }
                else cam

        goForward :: GLfloat -> Camera -> Camera
        goForward duration cam =
            if forward navigation
                then
                    let stride = duration * moveSpeed *^ direction cam
                    in cam { position = position cam + stride }
                else cam

        goBackward :: GLfloat -> Camera -> Camera
        goBackward duration cam =
            if backward navigation
                then
                    let stride = duration * moveSpeed *^ direction cam
                    in cam { position = position cam - stride }
                else cam


makeDirection :: GLfloat -> V3 GLfloat
makeDirection rot = rotate (axisAngle yAxis rot) north

makeViewMatrix :: Camera -> M44 GLfloat
makeViewMatrix camera =
    lookAt (position camera) (position camera + direction camera) (V3 0 1 0)

yAxis :: V3 GLfloat
yAxis = V3 0 1 0

north :: V3 GLfloat
north = V3 0 0 (-1)

moveSpeed :: GLfloat
moveSpeed = 8 -- Units per second.

rotateSpeed :: GLfloat
rotateSpeed = 0.4 -- Circles per second.

fullCircle :: GLfloat
fullCircle = 2 * pi
