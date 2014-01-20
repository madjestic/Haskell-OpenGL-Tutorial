--------------------------------------------------------------------------------
-- |
-- | Module      :  Data
-- | Copyright   :  (c) Vladimir Lopatin 2013
-- | License     :  BSD3
-- |
-- | Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- | Stability   :  experimental
-- | Portability :  untested
-- |
-- | The NGL library is based on the notion of drawable shapes. 
-- |             Triangles    -> Triangles         
-- | Editable -> Instanceable -> Drawable
-- |                   Shapes -> Picture
-- |
-- | basic shapes types should be of 2 kinds:
-- | Shapes  positioned by center
-- | Shapes' positioned by bottom-left corner--
--------------------------------------------------------------------------------


module NGL.Data where

import Graphics.Rendering.OpenGL (Vertex2(..))



data Instanceable = Circle Point Radius Divisions
                  | Square Point Side
                  | Line  (Point, Point) -- | Ordered pair to store directionality
                  deriving Show


data Editable = Triangle [Point]
              | Quad     [Point] -- | BL vertex TR vertex
              | Polygon  [Point] -- | [Triangle] ?
              | Curve    [Point]
              deriving Show
                       

type Drawable = [Vertex2 Float]


type Point     =(Float, Float)
type Radius    = Float
type Side      = Float
type Divisions = Int


vertex :: Point -> Vertex2 Float
vertex p = (\(k,l) -> Vertex2 k l) p


triangle ::  [Point] -> Editable
triangle = Triangle


square :: Point -> Float -> [(Float, Float)]
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

line :: (Point -> Point) -> Instanceable
line p1@(x1,y1) p2@(x2,y2) = length 

circle :: Point -> Float -> Int -> [(Float, Float)]
circle pos r div =
    let
        x = fst pos
        y = snd pos
        div'    = fromIntegral div
        sines   = map ((y +).(r *).sin) [0.0, 2*pi/div' .. 2*pi]
        cosines = map ((x +).(r *).cos) [0.0, 2*pi/div' .. 2*pi]
    in
     zip sines cosines



toDrawable :: Instanceable -> Drawable
toDrawable (Square pos side)  = 
    map vertex $ square pos side
toDrawable (Circle pos r div) = 
    let abbc (x:xs) = [x] ++ (concat $ map (\(x,y) -> [x,y]) $ map (\x -> (x, x)) (init xs)) ++ [last xs]
        insertpos (x:y:[]) = [[(0.0,0.0),x,y]]
        insertpos (x:y:xs) = [[(0.0,0.0),x,y]] ++ insertpos (xs)
    in 
     map vertex $ concat $ insertpos $ abbc $ circle pos r div

