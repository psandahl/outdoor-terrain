module Helpers
    ( makeTranslate
    ) where

import           Graphics.LWGL (GLfloat)
import           Linear        (M44, V3 (..), V4 (..))

makeTranslate :: V3 GLfloat -> M44 GLfloat
makeTranslate (V3 x y z) =
    V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)
