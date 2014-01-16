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
-- | The HNGL library is based on the notion of drawable shapes. 
-- |             Triangles    -> Triangles         
-- | Editable -> Instanceable -> Drawable
-- |                   Shapes -> Picture
-- |
-- | basic shapes types should be of 2 kinds:
-- | Shapes  positioned by center
-- | Shapes' positioned by bottom-left corner--
--------------------------------------------------------------------------------


module HNGL.Data where

import Graphics.Rendering.OpenGL (Vertex2(..))



data Instanceable = Circle Position Radius Divisions
                  | Square Position Side           
                  deriving Show


data Editable = Triangle [Point]
              | Quad     [Point] -- | BL vertex TR vertex
              | Polygon  [Point] -- | [Triangle] ?
              deriving Show
                       

type Drawable = [Vertex2 Float]


type Position  = (Float, Float)
type Point     = (Float, Float)
type Radius    = Float
type Side      = Float
type Divisions = Int



vertex :: Point -> Vertex2 Float
vertex p = (\(k,l) -> Vertex2 k l) p


triangle ::  [Point] -> Editable
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


circle :: Int -> [(Float, Float)]
circle div =
    let
        div'    = fromIntegral div
        sines   = map sin [0.0, 2*pi/div' .. 2*pi]
        cosines = map cos [0.0, 2*pi/div' .. 2*pi]
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
     map vertex $ concat $ insertpos $ abbc $ circle div

