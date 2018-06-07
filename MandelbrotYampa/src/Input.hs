{-# LANGUAGE MultiWayIf #-}

module Input
    ( AppInput
    , parseWinInput
    , mousePos
    , lbp
    , lbpPos
    , lbDown
    , rbp
    , rbpPos
    , rbDown
    , key
    , quitEvent
    , module SDL.Input.Keyboard.Codes
    ) where

import           Data.Maybe

import           FRP.Yampa

import           Linear (V2(..))
import           Linear.Affine (Point(..))

import           SDL.Input.Keyboard.Codes
import qualified SDL

import Debug.Trace as DT

-- <| Signal Functions |> --
-- | Current mouse position
mousePos :: SF AppInput (Double,Double)
mousePos = arr inpMousePos

-- | Events that indicate left button click
lbp :: SF AppInput (Event ())
lbp = lbpPos >>^ tagWith ()

-- | Events that indicate left button click and are tagged with mouse position
lbpPos :: SF AppInput (Event (Double,Double))
lbpPos = inpMouseLeft ^>> edgeJust

-- | Is left button down
lbDown :: SF AppInput Bool
lbDown = arr (isJust . inpMouseLeft)

-- | Events that indicate right button click
rbp :: SF AppInput (Event ())
rbp = rbpPos >>^ tagWith ()

-- | Events that indicate right button click and are tagged with mouse position
rbpPos :: SF AppInput (Event (Double,Double))
rbpPos = inpMouseRight ^>> edgeJust

-- | Is right button down
rbDown :: SF AppInput Bool
rbDown = arr (isJust . inpMouseRight)

key :: SDL.Scancode -> String -> SF AppInput (Event ())
key code mode
  | code == SDL.ScancodeUp   ||
    code == SDL.ScancodeDown ||
    code == SDL.ScancodeQ
    = (inpKeyMode ^>> edgeJust) >>^ filterE (code ==) >>^ tagWith ()
      where
       inpKeyMode
         = if | mode == "Pressed"
                -> inpKeyPressed
              | otherwise
                -> inpKeyReleased

quitEvent :: SF AppInput (Event ())
quitEvent = arr inpQuit >>> edge
-- | Exported as abstract type. Fields are accessed with signal functions.
-- | AppInput ~= AppInput
data AppInput =
     AppInput
     { inpMousePos    :: (Double, Double)       -- ^ Current mouse position
     , inpMouseLeft   :: Maybe (Double, Double) -- ^ Down button currently down
     , inpMouseRight  :: Maybe (Double, Double) -- ^ Right button currently down
     , inpQuit        :: Bool                   -- ^ SDL's QuitEvent
     , inpKeyPressed  :: Maybe SDL.Scancode
     , inpKeyReleased :: Maybe SDL.Scancode
     }

type WinInput = Event SDL.EventPayload    

initAppInput :: AppInput
initAppInput =
     AppInput
     { inpMousePos    = (0, 0)
     , inpMouseLeft   = Nothing
     , inpMouseRight  = Nothing
     , inpQuit        = False
     , inpKeyPressed  = Nothing
     , inpKeyReleased = Nothing
     }

-- | Filter and transform SDL events into events which are relevant to our
--   application
parseWinInput :: SF WinInput AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

scancode :: SDL.KeyboardEventData -> Scancode
scancode ev =
  SDL.keysymScancode $ SDL.keyboardEventKeysym ev

isPressHold :: SDL.KeyboardEventData -> Bool
isPressHold ev =
  (isPressed && hold)
    where
      isPressed = SDL.keyboardEventKeyMotion ev == SDL.Pressed
      hold  = SDL.keyboardEventRepeat    ev == True

isPressed :: SDL.KeyboardEventData -> Bool
isPressed ev =
  SDL.keyboardEventKeyMotion ev == SDL.Pressed

isReleased :: SDL.KeyboardEventData -> Bool
isReleased ev =
  SDL.keyboardEventKeyMotion ev == SDL.Released

nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
nextAppInput inp SDL.QuitEvent
  = inp { inpQuit = True }
nextAppInput inp (SDL.MouseMotionEvent ev) =
    inp { inpMousePos = (fromIntegral x, fromIntegral y) }
    where P (V2 x y) = SDL.mouseMotionEventPos ev
nextAppInput inp (SDL.KeyboardEvent ev)
    | scancode ev == SDL.ScancodeEscape
      = inp { inpQuit = True }
    | scancode ev == SDL.ScancodeUp   || 
      scancode ev == SDL.ScancodeDown ||
      scancode ev == SDL.ScancodeQ
      = if | SDL.keyboardEventKeyMotion ev == SDL.Pressed
           -> inp { inpKeyPressed  = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
                  , inpKeyReleased = Nothing }
           | otherwise
           -> inp { inpKeyPressed  = Nothing
                  , inpKeyReleased = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
nextAppInput inp (SDL.MouseButtonEvent ev) = inp { inpMouseLeft  = lmb
                                                 , inpMouseRight = rmb }
    where motion = SDL.mouseButtonEventMotion ev
          button = SDL.mouseButtonEventButton ev
          pos    = inpMousePos inp
          inpMod = case (motion,button) of
              (SDL.Released, SDL.ButtonLeft)  -> first (const Nothing)
              (SDL.Pressed, SDL.ButtonLeft)   -> first (const (Just pos))
              (SDL.Released, SDL.ButtonRight) -> second (const Nothing)
              (SDL.Pressed, SDL.ButtonRight)  -> second (const (Just pos))
              _                                      -> id
          (lmb,rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) inp

nextAppInput inp _ = inp
