{-# LANGUAGE Arrows #-}

module Graphics.RedViz.Input.FRP.Yampa.Update.Keyboard
  ( updateKeyboard
  ) where

import FRP.Yampa
import Data.Functor (($>))
import SDL hiding   ((*^), (^+^), (^-^), (^/), norm, Event, Mouse)

import Graphics.RedViz.Input.Keyboard
import Graphics.RedViz.Input.FRP.Yampa.AppInput

-- | ~inspired by foldrWith mtx0 keys - for every keyInput apply a folding transform to mtx0
updateKeyboard :: Keyboard -> SF (AppInput, Keyboard) (Keyboard, [Event ()])
updateKeyboard kbd0 =
  proc (input, kbd) -> do
        (keys', kevs) <- updateKeys (keys kbd0) -< (input, (keys kbd))
        let
          events = [(catEvents kevs) $> ()]
          kbd' = kbd { keys = keys' }

        returnA -< (kbd', events)

updateKeys :: Keys -> SF (AppInput, Keys) (Keys, [Event ()])
updateKeys keys0 =
  proc (input, _) -> do
    (keyW_, keyWe) <- keyEvent SDL.ScancodeW keyW keys0 -< input
    (keyS_, keySe) <- keyEvent SDL.ScancodeS keyS keys0 -< input
    (keyA_, keyAe) <- keyEvent SDL.ScancodeA keyA keys0 -< input
    (keyD_, keyDe) <- keyEvent SDL.ScancodeD keyD keys0 -< input

    (keyQ_, keyQe) <- keyEvent SDL.ScancodeQ keyQ keys0 -< input
    (keyE_, keyEe) <- keyEvent SDL.ScancodeE keyE keys0 -< input
    (keyZ_, keyZe) <- keyEvent SDL.ScancodeZ keyZ keys0 -< input
    (keyC_, keyCe) <- keyEvent SDL.ScancodeC keyC keys0 -< input
    (keyPageUp_,   keyPageUpE)   <- keyEvent SDL.ScancodePageUp   keyPageUp   keys0 -< input
    (keyPageDown_, keyPageDownE) <- keyEvent SDL.ScancodePageDown keyPageDown keys0 -< input

    (keyLShift_, keyLShiftE) <- keyEvent SDL.ScancodeLShift keyLShift keys0 -< input
    (keyLCtrl_ , keyLCtrlE)  <- keyEvent SDL.ScancodeLCtrl  keyLCtrl  keys0 -< input
    (keyLAlt_ , keyLAltE)    <- keyEvent SDL.ScancodeLAlt   keyLAlt   keys0 -< input

    (keyUp_,    keyUpE)    <- keyEvent SDL.ScancodeUp    keyUp    keys0 -< input
    (keyDown_,  keyDownE)  <- keyEvent SDL.ScancodeDown  keyDown  keys0 -< input
    (keyLeft_,  keyLeftE)  <- keyEvent SDL.ScancodeLeft  keyLeft  keys0 -< input
    (keyRight_, keyRightE) <- keyEvent SDL.ScancodeRight keyRight keys0 -< input

    let events = [      keyWe, keySe, keyAe, keyDe, keyQe, keyEe, keyZe, keyCe, keyUpE, keyDownE, keyLeftE,   keyRightE,    keyPageUpE, keyPageDownE, keyLShiftE, keyLCtrlE, keyLAltE ]
        keys'  = ( Keys keyW_  keyS_  keyA_  keyD_  keyQ_  keyE_  keyZ_  keyC_  keyUp_  keyDown_  keyLeft_    keyRight_     keyPageUp_  keyPageDown_  keyLShift_  keyLCtrl_  keyLAlt_ )

    returnA -< (keys', events)

keyEvent :: SDL.Scancode -> (Keys -> Bool) -> Keys -> SF AppInput (Bool, Event ())
keyEvent code keyFunc keys0 =
  proc input -> do
    keyPressed     <- keyInput code  "Pressed"  -< input
    keyReleased    <- keyInput code  "Released" -< input
    let
      result = keyState (keyFunc keys0) keyPressed keyReleased
      event' = lMerge keyPressed keyReleased
    returnA -< (result, event')

keyState :: Bool -> Event () -> Event () -> Bool
keyState state pressed released
  | isEvent pressed  = True
  | isEvent released = False
  | otherwise        = state
