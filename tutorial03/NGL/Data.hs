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
-- | The NGL library works, by dumping a vertex array into OpenGL buffer
-- |
-- | basic shapes types should be of 2 kinds:
-- | Shapes  positioned by center
-- | Shapes' positioned by bottom-left corner--
--------------------------------------------------------------------------------


module NGL.Shape where

import Graphics.Rendering.OpenGL (Vertex2(..))
import NGL.Maths as M


data Shape = Circle Point Radius Divisions
           | Square Point Side
           | Rect   Point Point
           | Line  (Point, Point) -- | Ordered pair to store directionality
           | Triangle [Point]
           | Quad     [Point] -- | BL vertex TR vertex
           | Polygon  [Point] -- | [Triangle] ?
           | Curve    [Point]
           deriving Show


-- type Drawable =
type Picture   =[Vertex2 Float]
type Point     =(Float, Float)
type Radius    = Float
type Side      = Float
type Divisions = Int

--picture :: [Shape] -> Picture
--picture xs = concat xs

picture :: Shape -> Picture
picture (Square pos len)     = square pos len
picture (Circle pos rad divs) = circle pos rad divs

vertex :: Point -> Vertex2 Float
vertex p = (\(k,l) -> Vertex2 k l) p


triangle ::  [Point] -> Shape
triangle = Triangle


square :: Point -> Float -> [Vertex2 Float]
square pos side = map vertex [p1, p2, p3,
                              p1, p3, p4]
    where          
        x = fst pos
        y = snd pos
        r = side/2 
        p1 = (x + r, y + r)
        p2 = (x - r, y + r)
        p3 = (x - r, y - r)
        p4 = (x + r, y - r)


circle :: Point -> Float -> Int -> [Vertex2 Float]
circle pos r divs =
    let
        x = fst pos
        y = snd pos
        divs'    = fromIntegral divs
        sines   = map ((y +).(r *).sin) [0.0, 2*pi/divs' .. 2*pi]
        cosines = map ((x +).(r *).cos) [0.0, 2*pi/divs' .. 2*pi]       
    in
        map vertex $ concat $ insertpos $ abbc $ zip sines cosines
            where
                  abbc (x:xs) = [x] ++ (concat $ map (\(x,y) -> [x,y]) $ map (\x -> (x, x)) (init xs)) ++ [last xs]
                  insertpos (x:y:[]) = [[(0.0,0.0),x,y]]
                  insertpos (x:y:xs) = [[(0.0,0.0),x,y]] ++ insertpos (xs)
