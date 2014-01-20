module Main where

import NGL.Shape
import NGL.Rendering
import NGL.TinyMath


main :: IO ()
main = do
     let prims = [  shape $ Square (-0.5, -0.5) 1.0
                  , shape $ Circle (0.5, 0.5) 0.5 10
                  , shape $ Rect (-1.0,0.33) (0.0,0.66)
                  , rotate (-45) $ shape $ Line (0.0,-0.5) (1.0,-0.5) 0.05
                  --, shape $ Polyline [(0.0,-0.5),(0.5,0.0),(1.0,-0.5)] 0.05 
                 ]
     draw $ toVertex2 prims 
