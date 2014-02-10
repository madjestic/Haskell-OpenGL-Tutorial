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
-- | The NGL library works, by dumping a vertex array into OpenGL buffer
-- |
-- | basic shapes types should be of 2 kinds:
-- | Shapes  positioned by center
-- | Shapes' positioned by bottom-left corner--
--------------------------------------------------------------------------------

module NGL.Shape where

import Graphics.Rendering.OpenGL (Vertex4(..),
                                  Color4(..),
                                  GLclampf(..))
import NGL.Utils


data Shape = Circle    Point   Radius Divisions
           | Square    Point   Side
           | Rect      Point   Point
           | Line      Point   Point  Float  -- | Ordered pair to store directionality
           | Triangle  Point   Point  Point
           | Quad     [Point]    -- | BL vertex TR vertex
           | Polygon  [Point]    -- | [Triangle] ? 
           | Polyline [Point]  Float
           | Curve    [Point]
           deriving Show


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


type Picture   =[Vertex4 Float]
type Points    =[Point]
type Point     =(Float, Float)
type Radius    = Float
type Side      = Float
type Divisions = Int
type Drawable  = ([Color4 Float],[Vertex4 Float])


toDrawable :: Color -> Shape -> Drawable
toDrawable clr x = (cs,vs)
    where
           vs    = map vertex $ shape x
           color = getColor clr
           cs    = map (\x -> color) $ vs

toVertex :: [Point] -> Picture
toVertex xs = map vertex xs

vertex :: Point -> Vertex4 Float
vertex p = (\(k,l) -> Vertex4 k l 0 1) p

rotate :: Float -> [(Float, Float)] -> [(Float, Float)]
rotate theta = rotate2D' (toRadians theta)

shape :: Shape -> [Point]
shape (Square   pos side)     =  square pos side
shape (Circle   pos rad divs) =  circle pos rad divs
shape (Rect     bl  tr)       =  rect   bl  tr        -- | bl := bottom left, tr := top right
shape (Line     p1  p2  w)    =  line   p1  p2  w
shape (Polyline ps  w)        =  polyline ps w
shape (Triangle p1  p2 p3)    =  triangle p1 p2 p3


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
--getColor (RGB r g b)    = Color4 r g b 1
--getColor (RGBA r g b a) = Color4 r g b a
getColor x 
    | x == Red   = Color4 1 0 0 1 
    | x == Green = Color4 0 1 0 1
    | x == Blue  = Color4 0 0 1 1
    | x == White = Color4 1 1 1 1
    | otherwise  = Color4 0 0 0 1
