module Main where

import NGL.Shape
import NGL.Rendering

main :: IO ()
main = do
     let prims = [  shape $ Square (-0.5, -0.5) 1.0
                  , shape $ Circle (0.5, 0.5) 0.5 100
                  , shape $ Rect (-1.0,0.33) (0.0,0.66)
                  , shape $ Polyline [ (0.0,-0.66)
                                      ,(0.33,-0.33)
                                      ,(0.66,-0.66)
                                      ,(1.0,-0.33)] 
                                      0.01 
                 ]
     win <- createWindow "My First Window" (512,512)
     drawInWindow win prims
     closeWindow win
