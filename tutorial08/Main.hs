{-# LANGUAGE Rank2Types #-}

module Main where

import NGL.Shape
import NGL.Rendering

import Control.Applicative
import Control.Monad
import Graphics.UI.GLFW (pollEvents, Window)
import Reactive.Banana as R
import Reactive.Banana.Frameworks
import Reactive.Banana.GLFW


main :: IO ()
main = do
     let drawables = [
                 -- | toDrawable Color $ Shape  Position Radius Divisions
                      toDrawable White $ Circle (0.0, 0.0) 0.5 100       
                     ]

     window <- createWindow "NGL is Not GLoss" (512,512)     
     withEventsIn window drawables
--     drawIn Default window drawables
     closeWindow window


parm :: Integer
parm = 2

foo :: IO Int
foo = return (1::Int)


withEventsIn :: Window -> [Drawable] -> IO ()
withEventsIn window ds = do
     handle <- windowHandler window             
     network <- compile $ do
         keyE <- keyEvent handle
         reactimate $ exit window <$ filterE (match Key'Escape) keyE
--         reactimate $ (drawFoo window ds) <$ filterE (match Key'H) keyE
         reactimate $ print <$> keyE
         c <- cursor handle TopLeft
--         drawFoo window ds
         reactimate $ putStrLn . ("Cursor: " ++) . show <$> cursorMove c
         let i = 1::Int
         let ecount = accumE 0 ((+i) <$ filterE (match Key'Up) keyE)
         reactimate $ fmap print ecount 
         reactimate $ (drawFoo window [ toDrawable White $ Circle (0.0, 0.0) 0.5 100] ) <$ filterE (match Key'H) keyE
         reactimate $ (drawFoo window [ toDrawable White $ Circle (0.0, 0.0) 1.0 50] ) <$ filterE (match Key'N) keyE


     actuate network -- >> return (1::Int) >>= \x -> print x -- | Maybe that's how I can pass a stateful object to the drawLoop?
     --drawIn Default window ds
     forever pollEvents

fromEvent :: Event t Int -> Int
fromEvent = undefined

drawFoo :: Window -> [Drawable] -> IO ()
drawFoo window ds = drawIn Default window ds

-- drawParm :: Window -> [Drawable] -> Int -> IO ()
-- drawParm window ds iter = undefined

sayHello :: IO ()
sayHello = print "hello!"

