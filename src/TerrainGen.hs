module TerrainGen
    ( makeTerrainMesh
    , makeTerrainMeshFromMap
    , constHeight
    , mapHeight
    ) where

import           Codec.Picture
import           Data.Vector.Storable                (Vector, (!))
import qualified Data.Vector.Storable                as Vec
import           Data.Vector.Storable.Mutable        (IOVector)
import qualified Data.Vector.Storable.Mutable        as MVec
import           Graphics.LWGL
import           Graphics.LWGL.Vertex_P_Norm_Col_Tex (Vertex (..))
import           Linear                              (V2 (..), V3 (..), cross,
                                                      normalize)


type HeightGen = Int -> Int -> GLfloat

makeTerrainMesh :: Int -> Int -> HeightGen -> IO Mesh
makeTerrainMesh rows cols height = do
    let vertices = gridVertices rows cols height
        indices' = gridIndices (fromIntegral rows) (fromIntegral cols)
    mVertices <- Vec.unsafeThaw vertices
    calculateNormals mVertices indices'
    vertices' <- normalizeNormals <$> Vec.unsafeFreeze mVertices
    buildFromVector StaticDraw vertices' indices'

{-makeTerrainMesh :: Int -> Int -> HeightGen -> IO Mesh
makeTerrainMesh rows cols height =
    buildFromVector StaticDraw (gridVertices rows cols height)
                               (gridIndices (fromIntegral rows) (fromIntegral cols))
-}

makeTerrainMeshFromMap :: FilePath -> IO (Either String Mesh)
makeTerrainMeshFromMap file = do
    eImg <- readImageRGB8 file
    case eImg of
        Right img ->
            Right <$> makeTerrainMesh (imageWidth img)
                                      (imageHeight img)
                                      (mapHeight img)
        Left err -> return $ Left err

gridVertices :: Int -> Int -> HeightGen -> Vector Vertex
gridVertices rows cols height =
    Vec.fromList $ concat $ for_ [0 .. rows - 1] $ \row ->
        for_ [0 .. cols - 1] $ \col ->
            Vertex
                { position = V3 (fromIntegral col)
                                (height row col)
                                (fromIntegral row)
                , normal = V3 0 0 0
                , color = V3 0 0 0
                , texCoord = V2 (fromIntegral col)
                                (fromIntegral $ rows - (row + 1))
                }

gridIndices :: GLuint -> GLuint -> Vector GLuint
gridIndices rows cols =
    Vec.fromList $ concat $ for_ [0 .. rows - 2] $ \row ->
        concat $ for_ [0 .. cols - 2] $ \col ->
            let myIndex = col + (row * cols)
            in [ myIndex, myIndex + cols, myIndex + 1
               , myIndex + 1, myIndex + cols, myIndex + 1 + cols
               ]

calculateNormals :: IOVector Vertex -> Vector GLuint -> IO ()
calculateNormals vertices indices' = go 0
    where
        go :: Int -> IO ()
        go index
            | index < Vec.length indices' = do
                let i1 = fromIntegral (indices' ! index)
                    i2 = fromIntegral (indices' ! (index + 1))
                    i3 = fromIntegral (indices' ! (index + 2))

                v1 <- MVec.read vertices i1
                v2 <- MVec.read vertices i2
                v3 <- MVec.read vertices i3

                let vec1 = position v2 - position v1
                    vec2 = position v3 - position v1
                    norm = normalize $ vec1 `cross` vec2
                --print norm

                MVec.write vertices i1 $ v1 {normal = norm + normal v1}
                MVec.write vertices i2 $ v2 {normal = norm + normal v2}
                MVec.write vertices i3 $ v3 {normal = norm + normal v3}

                go $ index + 3

            | otherwise = return ()


normalizeNormals :: Vector Vertex -> Vector Vertex
normalizeNormals = Vec.map (\v -> v {normal = normalize $ normal v})

constHeight :: GLfloat -> Int -> Int -> GLfloat
constHeight h _ _ = h

mapHeight :: Image PixelRGB8 -> Int -> Int -> GLfloat
mapHeight img x y = fromIntegral (greyScalePixel img x y) / heightScale

-- | Read a pixel from an RGB8, and assume it's grey scale (i.e. all
-- channels having the same value). Read the red channel and return the value.
greyScalePixel :: Image PixelRGB8 -> Int -> Int -> Pixel8
greyScalePixel img x y =
    let PixelRGB8 red _green _blue = pixelAt img x y
    in red

heightScale :: GLfloat
heightScale = 11

for_ :: [a] -> (a -> b) -> [b]
for_ = flip map
