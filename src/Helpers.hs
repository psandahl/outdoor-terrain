module Helpers
    ( makeTranslate
    , makeProjection
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44, V3 (..), V4 (..), perspective)

makeTranslate :: V3 GLfloat -> M44 GLfloat
makeTranslate (V3 x y z) =
    V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)

makeProjection :: Int -> Int -> M44 GLfloat
makeProjection width height =
    perspective (degToRad 45)
                ( fromIntegral width / fromIntegral height )
                0.001 10000

degToRad :: Float -> Float
degToRad deg = deg * (pi / 180)
