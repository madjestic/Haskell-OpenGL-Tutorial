module Main where

import NGL.Shape
import NGL.Rendering

draw' :: Drawable -> IO ()
draw' drawable = do
     inWindow <- createWindow "NGL is Not GLoss" (512,512)
     draw inWindow drawable
     closeWindow inWindow

main :: IO ()
main = do
     let drawable = toDrawable $ Square (-0.0, -0.0) 1.0
     draw' drawable
