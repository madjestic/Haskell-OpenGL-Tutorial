{-# LANGUAGE Arrows #-}

module Graphics.RedViz.Input.FRP.Yampa.Update.Mouse
  ( updateMouse
  ) where

import FRP.Yampa

import Graphics.RedViz.Input.FRP.Yampa.AppInput
import Graphics.RedViz.Input.Mouse

-- import Debug.Trace    as DT

updateMouse :: SF AppInput (Mouse, Event [(Int, Int)])
updateMouse =
  proc input -> do
    lmbE <- lbpPos       -< input
    rmbE <- rbpPos       -< input
    mmovE <- mouseMoving -< input

    mpos' <- mousePos    -< input
    rpos' <- mouseRelPos -< input

    let
      events = catEvents [lmbE, rmbE, mmovE]
      mouse' =
        Mouse
        (case isEvent lmbE of
           True -> Just $ fromEvent lmbE
           _    -> Nothing)
        (case isEvent rmbE of
           True -> Just $ fromEvent rmbE
           _    -> Nothing)
        mpos'
        --(DT.trace ("mpos' : " ++ show mpos') mpos')
        rpos'
        (isEvent mmovE)
        []
    returnA -< (mouse', events)

