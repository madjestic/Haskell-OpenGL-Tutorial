module Main where

import NGL.Shape
import NGL.Rendering

draw' :: Drawable -> IO ()
draw' drawable = do
     window <- createWindow "NGL is Not GLoss" (512,512)
     drawIn Default window drawable
     closeWindow window
     
picture :: Picture
picture = (Square (-0.0, -0.0) 1.0, "test.png")

main :: IO ()
main = do
     let drawable = toDrawable Default $ picture
     draw' drawable
