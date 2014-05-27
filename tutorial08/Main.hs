{-# LANGUAGE Rank2Types #-}

module Main where

import NGL.Shape
import NGL.Rendering

import Control.Applicative
import System.Exit       ( exitSuccess )
import Graphics.UI.GLFW (Window)
import Reactive.Banana as R
import Reactive.Banana.Frameworks
import Reactive.Banana.GLFW


main :: IO ()
main = do
     let drawables = [
                      toDrawable White $ Circle (0.0, 0.0) 0.5 100       
                     ]

     window <- createWindow "NGL is Not GLoss" (512,512)     
     withEventsIn window     
     drawIn Default window drawables
     closeWindow window


withEventsIn :: Window -> IO ()
withEventsIn window = do
     h <- windowHandler window
     network <- compile $ do
         keyE <- keyEvent h
         reactimate $ exit window <$ filterE (match Key'Escape) keyE
         reactimate $ foo <$ filterE (match Key'H) keyE
         reactimate $ print <$> keyE
         c <- cursor h TopLeft
         reactimate $ putStrLn . ("Cursor: " ++) . show <$> cursorMove c
     actuate network


foo :: IO ()
foo = putStrLn "hello!"

exit :: Window -> IO ()
exit win = 
     do
      shutdown win
      exitSuccess
