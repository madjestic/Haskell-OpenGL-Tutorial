--------------------------------------------------------------------------------
-- |
-- Module      :  Camera
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A structure for a user-controllable object.
--
--------------------------------------------------------------------------------


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.RedViz.Controllable
  ( Controllable (..)
  , Device (..)
  , Keyboard (..)
  , Mouse (..)
  , transform
  , ypr
  , yprS
  , vel
  , device
  , device'
--  , mouse
  , keyboard
  ) where

import Linear.Matrix
import Linear.V3
import Control.Lens hiding (transform)

import Graphics.RedViz.Input.Keyboard
import Graphics.RedViz.Input.Mouse
import Graphics.RedViz.Utils ()

-- import Debug.Trace as DT

data Controllable
  =  Controller
     {
       _debug      :: (Int, Int)
     , _transform  :: M44 Double
     , _vel        :: V3 Double  -- velocity
     , _ypr        :: V3 Double  -- yaw/pitch/roll
     , _yprS       :: V3 Double  -- yaw/pitch/roll Sum
     , _device     :: Device     -- store as index in the proj file: 0 - keyboard, 1 - mouse, etc.
     }
  deriving Show

data Device
  =  Device
     {
       _keyboard :: Keyboard
     --, _mouse    :: Mouse
     } deriving Show

device'    :: Lens' Controllable Device
device'    = lens _device (\controllable newDevice    -> controllable { _device    = newDevice })

$(makeLenses ''Device)
$(makeLenses ''Controllable)
