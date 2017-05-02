module Main where

import NGL.Shape
import NGL.Rendering

main :: IO ()
main = do
     let drawables =
           [toDrawable Red     $ Square (-0.5, -0.5) 1.0,
                       toDrawable Green   $ Circle (0.5, 0.5) 0.5 100,
                       toDrawable Blue    $ Rect (-1.0,0.33) (0.0,0.66),
                       toDrawable White   $ Polyline [ (0.0,-0.66)
                                                       ,(0.33,-0.33)
                                                       ,(0.66,-0.66)
                                                       ,(1.0,-0.33)] 
                       0.01 
                       ]

     window <- createWindow "NGL is Not GLoss" (512,512)
     drawIn Default window drawables
     closeWindow window

