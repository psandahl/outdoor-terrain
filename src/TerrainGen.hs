module TerrainGen
    ( makeTerrainMesh
    , makeTerrainMeshFromMap
    , constHeight
    , constColor
    , mapHeight
    , mapColor
    ) where

import           Codec.Picture                           (Image (..), Pixel8,
                                                          PixelRGB8 (..),
                                                          pixelAt)
import           Data.Vector.Storable                    (Vector, (!))
import qualified Data.Vector.Storable                    as Vec
import           Data.Vector.Storable.Mutable            (IOVector)
import qualified Data.Vector.Storable.Mutable            as MVec
import           Graphics.LWGL
import           Graphics.LWGL.Vertex_P_Norm_Tan_Col_Tex (Vertex (..))
import           Linear                                  (V2 (..), V3 (..),
                                                          cross, normalize)


type HeightGen = Int -> Int -> GLfloat
type ColorGen = Int -> Int -> V3 GLfloat

makeTerrainMesh :: Int -> Int -> HeightGen -> ColorGen -> IO Mesh
makeTerrainMesh rows cols height color' = do
    let vertices = gridVertices rows cols height color'
        indices' = gridIndices (fromIntegral rows) (fromIntegral cols)
    mVertices <- Vec.unsafeThaw vertices
    calculateVectors mVertices indices'
    vertices' <- normalizeVectors <$> Vec.unsafeFreeze mVertices
    --writeFile "/tmp/vertices.txt" $ show vertices'
    buildFromVector StaticDraw vertices' indices'

{-makeTerrainMesh :: Int -> Int -> HeightGen -> IO Mesh
makeTerrainMesh rows cols height =
    buildFromVector StaticDraw (gridVertices rows cols height)
                               (gridIndices (fromIntegral rows) (fromIntegral cols))
-}

makeTerrainMeshFromMap :: FilePath -> FilePath -> IO (Either String Mesh)
makeTerrainMeshFromMap heightFile colorFile = do
    eImages <- sequence <$> mapM readImageRGB8 [heightFile, colorFile]
    case eImages of
        Right [heightMap, colorMap] ->
            Right <$> makeTerrainMesh (imageWidth heightMap)
                                      (imageHeight heightMap)
                                      (mapHeight heightMap)
                                      (mapColor colorMap)
        Right _ -> return $ Left "Wrong number of files"
        Left err -> return $ Left err

gridVertices :: Int -> Int -> HeightGen -> ColorGen -> Vector Vertex
gridVertices rows cols height color' =
    Vec.fromList $ concat $ for_ [0 .. rows - 1] $ \row ->
        for_ [0 .. cols - 1] $ \col ->
            Vertex
                { position = V3 (fromIntegral col)
                                (height row col)
                                (fromIntegral row)
                , normal = V3 0 0 0
                , tangent = V3 0 0 0
                , color = color' row col
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

calculateVectors :: IOVector Vertex -> Vector GLuint -> IO ()
calculateVectors vertices indices' = go 0
    where
        go :: Int -> IO ()
        go index
            | index < Vec.length indices' = do
                let i0 = fromIntegral (indices' ! index)
                    i1 = fromIntegral (indices' ! (index + 1))
                    i2 = fromIntegral (indices' ! (index + 2))

                v0 <- MVec.read vertices i0
                v1 <- MVec.read vertices i1
                v2 <- MVec.read vertices i2

                let edge1 = position v1 - position v0
                    edge2 = position v2 - position v0
                    deltaU1 = texCoord v1 `uSub` texCoord v0
                    deltaV1 = texCoord v1 `vSub` texCoord v0
                    deltaU2 = texCoord v2 `uSub` texCoord v0
                    deltaV2 = texCoord v2 `vSub` texCoord v0
                    f = 1.0 / (deltaU1 * deltaV2 - deltaU2 * deltaV1)

                    norm = normalize $ edge1 `cross` edge2
                    tang = normalize $
                        V3 (f * (deltaV2 * posX edge1 - deltaV1 * posX edge2))
                           (f * (deltaV2 * posY edge1 - deltaV1 * posY edge2))
                           (f * (deltaV2 * posZ edge1 - deltaV1 * posZ edge2))

                MVec.write vertices i0 $ v0 { normal = norm + normal v0
                                            , tangent = tang + tangent v0
                                            }
                MVec.write vertices i1 $ v1 { normal = norm + normal v1
                                            , tangent = tang + tangent v1
                                            }
                MVec.write vertices i2 $ v2 { normal = norm + normal v2
                                            , tangent = tang + tangent v2
                                            }

                go $ index + 3

            | otherwise = return ()

uSub :: V2 GLfloat -> V2 GLfloat -> GLfloat
uSub (V2 u1 _) (V2 u2 _) = u1 - u2

vSub :: V2 GLfloat -> V2 GLfloat -> GLfloat
vSub (V2 _ v1) (V2 _ v2) = v1 - v2

posX :: V3 GLfloat -> GLfloat
posX (V3 x _ _) = x

posY :: V3 GLfloat -> GLfloat
posY (V3 _ y _) = y

posZ :: V3 GLfloat -> GLfloat
posZ (V3 _ _ z) = z

normalizeVectors :: Vector Vertex -> Vector Vertex
normalizeVectors =
    Vec.map (\v -> v { normal = normalize $ normal v
                     , tangent = normalize $ tangent v}
            )

constHeight :: GLfloat -> Int -> Int -> GLfloat
constHeight h _ _ = h

constColor :: V3 GLfloat -> Int -> Int -> V3 GLfloat
constColor c _ _ = c

mapHeight :: Image PixelRGB8 -> Int -> Int -> GLfloat
mapHeight img x y = fromIntegral (greyScalePixel img x y) / heightScale

-- | Read a pixel from an RGB8, and assume it's grey scale (i.e. all
-- channels having the same value). Read the red channel and return the value.
greyScalePixel :: Image PixelRGB8 -> Int -> Int -> Pixel8
greyScalePixel img x y =
    let PixelRGB8 red _green _blue = pixelAt img x y
    in red

mapColor :: Image PixelRGB8 -> Int -> Int -> V3 GLfloat
mapColor img x y =
    let PixelRGB8 red green blue = pixelAt img x y
        toColor c = fromIntegral c / 255.0
    in V3 (toColor red) (toColor green) (toColor blue)

heightScale :: GLfloat
heightScale = 11

for_ :: [a] -> (a -> b) -> [b]
for_ = flip map
