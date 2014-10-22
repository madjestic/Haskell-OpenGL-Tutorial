module Main where

import NGL.Shape
import NGL.Rendering

main :: IO ()
main = do
     let drawables = toMinimal Red $ Square (-0.0, -0.0) 1.0

     window <- createWindow "NGL is Not GLoss" (512,512)
     drawIn Default window drawables
     closeWindow window
