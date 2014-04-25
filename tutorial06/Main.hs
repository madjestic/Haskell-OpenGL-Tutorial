module Main where

import NGL.Shape
import NGL.Rendering
import TinyMath.TinyMath
import Data.Numbers.Primes

scale :: Float
scale = 0.008

fromSpiral :: (Float, Float, Int) -> Drawable
fromSpiral (x,y,k) 
                | isPrime  k = toDrawable Red  $ Square (x, y) scale
                | otherwise  = toDrawable Blue $ Square (x, y) scale

main :: IO ()
main = do   
     let scaledSpiral = map (\(a,b,c) -> (scale*a,scale*b,c)) $ walkSpiral (0,0) (round $ 1/scale)
     let primeSpiral = map fromSpiral scaledSpiral

     window <- createWindow "NGL is Not GLoss" (900,900)
     drawIn Default window primeSpiral
     closeWindow window
