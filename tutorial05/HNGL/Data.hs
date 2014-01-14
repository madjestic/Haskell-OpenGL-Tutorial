module HNGL.Data where

import Graphics.Rendering.OpenGL (Vertex2(..))
import Data.Angle


-- | Libraty works like this:
-- |             Triangles   -> Triangles         
-- | Editable -> Instancable -> Drawabale
-- |                  Shapes -> Pictures

-- | basic shapes types should be of 2 kinds:
-- | Shapes  positioned by center
-- | Shapes' positioned by bottom-left corner
data Instanceable = Circle Position Radius Divisions
                  | Square Position Side           
                  deriving Show


data Editable = Triangle [Vertex]
              | Quad     [Vertex] -- | BL vertex TR vertex
              | Polygon  [Vertex] -- | [Triangle] ?
              deriving Show
                       

type Vertex   = Vertex2 Float
type Drawable = [Vertex]

-- type Triangles = [Triangle]
type Position  = (Float, Float)
type Point     = (Float, Float)
type Radius    = Float
type Side      = Float
type Divisions = Int


vertex :: Float -> Float -> Vertex
vertex = Vertex2


triangle ::  [Vertex] -> Editable
triangle = Triangle


square :: Position -> Float -> [(Float, Float)]
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


toTuple (x:y:[]) = [(x,y)]
toTuple (x:y:xs) = [(x,y)] ++ toTuple (xs)

append (x:y:[]) = [[(0.0,0.0),x,y]]
append (x:y:xs) = [[(0.0,0.0),x,y]] ++ append (xs)

foo (x:xs) = [x] ++ (concat $ map (\(x,y) -> [x,y]) $ map (\x -> (x, x)) (init xs)) ++ [last xs]

bar = append $ foo $ circle 10

-- [pos, p0, p1,
--  pos, p1, p2,
--  pos, p2, p3]

circle div =
    let
        sines   = map sin [0.0, 2*pi/div .. 2*pi]
        cosines = map cos [0.0, 2*pi/div .. 2*pi]
    in
     zip sines cosines 

toDrawable :: Instanceable -> Drawable
toDrawable (Square pos side)  = 
    let arr = unzip $ square pos side
    in uncurry (zipWith vertex) arr
toDrawable (Circle pos r div) =
    map (\(x,y) -> vertex x y) $ concat $ bar

-- toDrawable (Square pos side) =
--     map vertex [ (-0.90, -0.90), 
--                  ( 0.90, -0.90),
--                  ( 0.90,  0.90) ]

v1 = vertex (-0.90) (-0.90)
v2 = vertex 0.85  (-0.90)
v3 = vertex (-0.90)   0.85
