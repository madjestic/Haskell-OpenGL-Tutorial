module Main where

import NGL.Shape
import NGL.Rendering

main :: IO ()
main = do
     let prims = [  shape $ Triangle (0.0,1.0) (-1.0,-1.0) (1.0,-1.0)                
                 ]
     win <- createWindow "My First Window" (512,512)
     drawInWindow win prims
     closeWindow win
