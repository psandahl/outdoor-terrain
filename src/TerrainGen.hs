module TerrainGen
    ( makeTerrainMesh
    , constHeight
    ) where

import           Graphics.LWGL
import           Graphics.LWGL.Vertex_P (Vertex (..))
import           Linear                 (V3 (..))

type HeightGen = Int -> Int -> GLfloat

makeTerrainMesh :: Int -> Int -> HeightGen -> IO Mesh
makeTerrainMesh rows cols height =
    buildFromList StaticDraw (gridVertices rows cols height)
                             (gridIndices (fromIntegral rows) (fromIntegral cols))

gridVertices :: Int -> Int -> HeightGen -> [Vertex]
gridVertices rows cols height =
    concat $ for_ [0 .. rows - 1] $ \row ->
        for_ [0 .. cols - 1] $ \col ->
            Vertex
                { position = V3 (fromIntegral col)
                                (height row col)
                                (fromIntegral row)
                }

gridIndices :: GLuint -> GLuint -> [GLuint]
gridIndices rows cols =
    concat $ for_ [0 .. rows - 2] $ \row ->
        concat $ for_ [0 .. cols - 2] $ \col ->
            let myIndex = col + (row * cols)
            in [ myIndex, myIndex + cols, myIndex + 1
               , myIndex + 1, myIndex + cols, myIndex + 1 + cols
               ]

constHeight :: GLfloat -> Int -> Int -> GLfloat
constHeight h _ _ = h

for_ :: [a] -> (a -> b) -> [b]
for_ = flip map
