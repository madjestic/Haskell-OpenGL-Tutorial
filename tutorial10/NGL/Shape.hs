-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
--------------------------------------------------------------------------------
-- |
-- | Module      :  Data
-- | Copyright   :  (c) Vladimir Lopatin 2014
-- | License     :  BSD3
-- |
-- | Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- | Stability   :  experimental
-- | Portability :  untested
-- |
-- | The NGL library works, by dumping a toVertex4 array into OpenGL buffer
-- |
-- | basic shapes types should be of 2 kinds:
-- | Shapes  positioned by center
-- | Shapes' positioned by bottom-left corner--
--------------------------------------------------------------------------------

module NGL.Shape where

import Graphics.Rendering.OpenGL (Vertex4(..),
                                  Color4(..),
                                  TexCoord4(..),
                                  TexCoord2(..),
                                  GLclampf(..))
import NGL.Utils
import TinyMath.TinyMath


data Shape = Circle    Point   Radius Divisions
           | Square    Point   Side
           | Rect      Point   Point
           | Line      Point   Point  Float  -- | Ordered pair to store directionality
           | Triangle  Point   Point  Point
           | Quad     [Point]    -- | BL toVertex4 TR toVertex4
           | Polygon  [Point]    -- | [Triangle] ? 
           | Polyline [Point]  Float
           | Curve    [Point]
           deriving Show
           
type Picture = (Shape, Texture)

data Color = Red
            | Green
            | Blue
            | White
            | Black
            | RGB    GLclampf GLclampf GLclampf 
            | RGBA   GLclampf GLclampf GLclampf GLclampf
            | Default
            deriving Show

instance Eq Color where
    Red          == Red   = True
    Green        == Green = True
    Blue         == Blue  = True
    White        == White = True
    Black        == Black = True
    RGB _ _ _    == RGB _ _ _    = True
    RGBA _ _ _ _ == RGBA _ _ _ _ = True
    Default      == Default      = True
    _ == _ = False


data Transform = Rotate2D Float Point 
               | Translate2D Point Point
               deriving Show

type VertexArray =[Vertex4 Float]
type UV          =[TexCoord2 Float]
type Points      =[Point]
type Point       =(Float, Float)
type Radius      = Float
type Side        = Float
type Divisions   = Int
type Texture     = String

-- toMinimal :: Color -> Shape -> Shape
-- toMinimal clr x = (cs,vs)
--     where
--            vs    = map toVertex4 $ toPoints x
--            color = getColor clr
--            cs    = map (\x -> color) $ vs

-- toPicture :: Color -> Shape -> Picture -- it's more like a UV projection
-- toPicture clr x = (cs,vs,uvs)
--     where
--            vs    = map toVertex4 $ toPoints x
--            color = getColor clr
--            cs    = map (\x -> color) $ vs
--            uvs   = toTextureCoord2 ps
--                where ps = [(1.0, 1.0),(0.0, 1.0),(0.0, 0.0)
--                           ,(1.0, 1.0),(0.0, 0.0),(1.0, 0.0)]::Points

-- toDrawable :: Color -> Shape -> Primitive
-- toDrawable clr x = (cs,vs)
--     where
--            vs    = map vertex $ toPoints x
--            color = getColor clr
--            cs    = map (\x -> color) $ vs

type Drawable = ([Color4 Float],[Vertex4 Float],[TexCoord2 Float],String)

class Primitive a where 
      toDrawable :: Color -> a -> Drawable
      toPoints   :: a -> Points
instance Primitive Shape where
         toDrawable :: Color ->  Shape -> Drawable
         toDrawable clr x = (cs, vs, uv, tex)
                    where
                      vs'   = toPoints x
                      color = getColor clr
                      cs    = map (\_ -> color) vs'
                      uv    = map toTexCoord2 vs'
                      vs    = map toVertex4 $ vs'
                      tex   = "test.png"
                      
         toPoints :: Shape -> [Point]
         toPoints (Square   pos side)     =  square pos side
         toPoints (Circle   pos rad divs) =  circle pos rad divs
         toPoints (Rect     bl  tr)       =  rect   bl  tr        -- | bl := bottom left, tr := top right
         toPoints (Line     p1  p2  w)    =  line   p1  p2  w
         toPoints (Polyline ps  w)        =  polyline ps w
         toPoints (Triangle p1  p2 p3)    =  triangle p1 p2 p3

instance Primitive Picture where
         toDrawable :: Color ->  Picture -> Drawable
         toDrawable clr x = (cs, vs, uv, tex)
                    where
                      vs'   = toPicture x
                      color = getColor clr
                      cs    = map (\_ -> color) vs'
                      uv    = map toTexCoord2 vs'
                      vs    = map toVertex4 $ vs'
                      tex   = "test.png"
                      
         toPoints :: Picture -> [Point]
         toPoints = undefined

toVertexArray :: [Point] -> VertexArray
toVertexArray xs = map toVertex4 xs

toVertex4 :: Point -> Vertex4 Float
toVertex4 p = (\(k,l) -> Vertex4 k l 0 1) p

toTextureCoord2 :: [Point] -> UV
toTextureCoord2 xs = map (\(k,l) -> TexCoord2 k l) xs

toTexCoord2 :: (a, a) -> TexCoord2 a
toTexCoord2 p = (\(k,l) -> TexCoord2 k l) p

toTextureCoord4 :: Point -> TexCoord4 Float
toTextureCoord4 p = (\(k,l) -> TexCoord4 k l 0 0) p

rotate :: Float -> [(Float, Float)] -> [(Float, Float)]
rotate theta = rotate2D' (toRadians theta)

toPicture :: Picture -> [Point]
toPicture = undefined

data Projection = Planar                
                deriving Show

toUV :: Projection -> UV
toUV Planar = toTextureCoord2 ps
                  where ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
                             ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)]::Points


polyline :: [Point] -> Float -> [Point]
polyline ps w = concatMap (\(x,y) -> line x y w) $ pairs $ abbcca ps


triangle :: Point -> Point -> Point -> [Point]
triangle p1 p2 p3 = [p1, p2, p3]


square :: Point -> Float -> [Point]
square pos side = [p1, p2, p3,
                   p1, p3, p4]
    where          
        x = fst pos
        y = snd pos
        r = side/2 
        p1 = (x + r, y + r)
        p2 = (x - r, y + r)
        p3 = (x - r, y - r)
        p4 = (x + r, y - r)
        
-- | abbcca := [a,b,c] -> [a,b,b,c]
abbcca :: [a] -> [a]
abbcca (x:xs) = [x] ++ (concat $ map (\(x,y) -> [x,y]) $ map (\x -> (x, x)) (init xs)) ++ [last xs]


circle :: Point -> Float -> Int -> [Point]
circle pos r divs =
    let
        x = fst pos
        y = snd pos
        divs'    = fromIntegral divs
        sines   = map ((y +).(r *).sin) [0.0, 2*pi/divs' .. 2*pi]
        cosines = map ((x +).(r *).cos) [0.0, 2*pi/divs' .. 2*pi]       
    in
        concat $ insertpos $ abbcca $ zip sines cosines
            where
                  insertpos (x:y:[]) = [[pos,x,y]]
                  insertpos (x:y:xs) = [pos,x,y] : insertpos xs


rect :: Point -> Point -> [Point]
rect (x1,y1) (x2,y2) = [(x2,y2),(x1,y2),(x1,y1),
                        (x2,y2),(x1,y1),(x2,y1)]


line :: Point -> Point -> Float -> [Point]
line (x1,y1) (x2,y2) w = map (addVectors (x1,y1)) $ rotate2D' theta $ rect (0.0,-w/2) (len,w/2) -- rotation is wrong
     where 
           (x,y) = normalize $ ((x2-x1),(y2-y1))
           theta = signum y * acos x                               -- | angle in radians
           len   = sqrt((x2-x1)^2+ (y2-y1)^2)

getColor :: (Real a) => Color -> Color4 a
getColor clr 
    | clr == Red   = Color4 1 0 0 1 
    | clr == Green = Color4 0 1 0 1
    | clr == Blue  = Color4 0 0 1 1
    | clr == White = Color4 1 1 1 1
    | otherwise  = Color4 0 0 0 1


data BBOX = BL
          | TR
          deriving Show

-- | bbox := Primitive -> (bl,tr) 
-- |   where bl is bottom left, tr is top right
bbox :: [Point] -> (Point, Point)
bbox k = (bl, tr)
       where 
           bl = (\[x,y] -> (x,y)) $ map head $ map qsort $ (\(xs,ys) -> [xs,ys]) $ unzip k
           tr = (\[x,y] -> (x,y)) $ map head $ map reverse $ map qsort $ (\(xs,ys) -> [xs,ys]) $ unzip k
-- bbox is sort x, sort y, zip pairs: [(x0,y0),(x1,y1),(x2,y2)...]->([x0,x1,x2,...][y0,y1,y2,...])


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
      where
        lesser  = [x | x <- xs, x < p ]
        greater = [x | x <- xs, x >= p]
