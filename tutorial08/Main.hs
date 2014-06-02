{-# LANGUAGE Rank2Types #-}

module Main where

import NGL.Shape
import NGL.Rendering

import Control.Applicative
import Control.Monad
import Graphics.UI.GLFW (pollEvents, Window)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GLFW
-- import qualified Reactive.Banana.Internal.Combinators as Prim
-- import Data.Reactive


main :: IO ()
main = do
     let drawables = [
                 -- | toDrawable Color $ Shape  Position Radius Divisions
                      toDrawable White $ Circle (0.0, 0.0) 0.5 100       
                     ]

     window <- createWindow "NGL is Not GLoss" (512,512)     
     drawIn Default window drawables
     withEventsIn window drawables
     closeWindow window


withEventsIn :: Window -> [Drawable] -> IO ()
withEventsIn window ds = do
     handle <- windowHandler window             
     network <- compile $ do
         keyE <- keyEvent handle
         reactimate $ exit window <$ filterE (match Key'Escape) keyE
         reactimate $ print <$> keyE
         c <- cursor handle TopLeft
         reactimate $ putStrLn . ("Cursor: " ++) . show <$> cursorMove c
         let i = 1::Int
         let ecount = accumE 0 ((+i) <$ filterE (match Key'Up) keyE)
         reactimate $ fmap print ecount 
         reactimate $ fmap (\x->   ( drawFoo window [ toDrawable White $ Circle (0.0, 0.0) 1.0 (x+3)] )   ) ecount
         reactimate $ (drawFoo window [ toDrawable White $ Circle (0.0, 0.0) 0.5 10] ) <$ filterE (match Key'Down) keyE


     actuate network
     forever pollEvents

fromEvent :: Event t Int -> Int
fromEvent = undefined


drawFoo :: Window -> [Drawable] -> IO ()
drawFoo window ds = drawIn Default window ds


sayHello :: IO ()
sayHello = print "hello!"

