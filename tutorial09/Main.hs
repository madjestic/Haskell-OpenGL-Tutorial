{-# LANGUAGE Rank2Types #-}

module Main where

import NGL.Shape
import NGL.Rendering

import Control.Applicative
import Control.Monad
import Graphics.UI.GLFW (pollEvents, getTime, Window)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GLFW


main :: IO ()
main = do
     let drawables = [ toDrawable White $ Circle (0.0, 0.0) 0.5 3 ]

     window <- createWindow "NGL is Not GLoss" (512,512)     
     drawIn Default window drawables
     withEventsIn window drawables
     closeWindow window


withEventsIn :: Window -> [Drawable] -> IO ()
withEventsIn window ds = do
     handle <- windowHandler window             
     network <- compile $ do
     
         -- | Keyboard events
         keyE <- keyEvent handle
         reactimate $ exit window <$ filterE (match Key'Escape) keyE
         reactimate $ print <$> keyE
         
         let ecount = accumE 0 $ ((+1) <$ filterE (match Key'Up) keyE) `union` ((subtract 1) <$ filterE (match Key'Down) keyE)
         reactimate $ fmap (\x->   ( drawIn Default window [ toDrawable White $ Circle (0.0, 0.0) 1.0 (x+3)] )   ) ecount
         --t <- maybe 0 id <$> getTime
--         let t = getTime
--         reactimate $ putStrLn (maybe t) <$ filterE (match Key'T) keyE

         -- | Mouse events:
         c <- cursor handle TopLeft
         reactimate $ putStrLn . ("Cursor: " ++) . show <$> cursorMove c
         
     actuate network
     forever pollEvents
