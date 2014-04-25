module Main where

import NGL.Shape
import NGL.Rendering
import NGL.Utils
-- import NGL.Patterns
import Data.Numbers.Primes

-- | p2
-- | |  \
-- | p3- p1
serpinsky :: [Shape] -> [Shape]
serpinsky [] = []
serpinsky ((Triangle p1 p2 p3):xs) = (Triangle p1 p2 p3) : serpinsky xs

midPoint :: Point -> Point -> Point
midPoint p1 p2 = (p2-p1)/2 + p1 -- I need to define (/) for Point and a Float

copy :: Shape -> Float -> Point -> [Point]
copy name s p = offset p $ scale s $ shape name

main :: IO ()
main = do   

     let picture = [ toDrawable Red $ Triangle (1.0,-1.0) (-1.0,1.0) (-1.0,-1.0) ]
     window <- createWindow "NGL is Not GLoss" (900,900)
     drawIn Default window picture
     closeWindow window

