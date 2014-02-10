module Main where

import NGL.Shape
import NGL.Rendering
import TinyMath.TinyMath


-- | 0.01
scale :: Float
scale = 0.01

mscale :: Float
mscale 
       | scale == 0.01 = 1
       | otherwise = 0.7

fromSpiral :: RealFrac a => (Float, Float, a) -> Drawable
fromSpiral (x,y,k) 
                | isPrime1 k = toDrawable Red  $ Square (x, y) (scale*mscale)
                | otherwise  = toDrawable Blue $ Square (x, y) (scale*mscale)

main :: IO ()
main = do
     let primeSpiral = map fromSpiral $ map (\(a,b,c) -> (scale*a,scale*b,c)) $ walkSpiral (0,0) 100
     let drawables = [toDrawable Red     $ Square (-0.5, -0.5) 1.0,
                      toDrawable Green   $ Circle (0.5, 0.5) 0.5 100,
                      toDrawable Blue    $ Rect (-1.0,0.33) (0.0,0.66),
                      toDrawable White   $ Polyline [ (0.0,-0.66)
                                                             ,(0.33,-0.33)
                                                             ,(0.66,-0.66)
                                                             ,(1.0,-0.33)] 
                                                             0.01 
                     ]

     window <- createWindow "NGL is Not GLoss" (900,900)
     drawIn Default window primeSpiral
     closeWindow window

