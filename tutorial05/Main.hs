module Main where

import NGL.Shape
import NGL.Rendering

main :: IO ()
main = do
     let drawable = toDrawable Green $ Triangle (0.0,1.0) (-1.0,-1.0) (1.0,-1.0)
     window <- createWindow "My First Window" (512,512)
     drawIn White window drawable
     closeWindow window

