module TerrainGen
    ( makeTerrainMesh
    , makeTerrainMeshFromMap
    , constHeight
    , mapHeight
    ) where

import           Codec.Picture
import           Data.Vector.Storable            (Vector)
import qualified Data.Vector.Storable            as Vec
import           Graphics.LWGL
import           Graphics.LWGL.Vertex_P_Norm_Tex (Vertex (..))
import           Linear                          (V2 (..), V3 (..))


type HeightGen = Int -> Int -> GLfloat

makeTerrainMesh :: Int -> Int -> HeightGen -> IO Mesh
makeTerrainMesh rows cols height =
    buildFromVector StaticDraw (gridVertices rows cols height)
                               (gridIndices (fromIntegral rows) (fromIntegral cols))

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

constHeight :: GLfloat -> Int -> Int -> GLfloat
constHeight h _ _ = h

mapHeight :: Image PixelRGB8 -> Int -> Int -> GLfloat
mapHeight img x y = (fromIntegral $ greyScalePixel img x y) / heightScale

-- | Read a pixel from an RGB8, and assume it's grey scale (i.e. all
-- channels having the same value). Read the red channel and return the value.
greyScalePixel :: Image PixelRGB8 -> Int -> Int -> Pixel8
greyScalePixel img x y =
    let PixelRGB8 red _green _blue = pixelAt img x y
    in red

heightScale :: GLfloat
heightScale = 12

for_ :: [a] -> (a -> b) -> [b]
for_ = flip map
