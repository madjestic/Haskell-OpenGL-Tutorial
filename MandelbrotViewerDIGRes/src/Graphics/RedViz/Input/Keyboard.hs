--------------------------------------------------------------------------------
-- |
-- Module      :  Keyboard
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic keyboard control structure.
--
--------------------------------------------------------------------------------
module Graphics.RedViz.Input.Keyboard
  ( Keyboard (..)
  , Keys (..)
  ) where

import Linear.V3

data Keyboard
  =  Keyboard
  { -- | Keyboard State
    keys    :: Keys
  , keyVecs :: [V3 Double]
  } deriving Show

data Keys =
     Keys
     { keyW        :: Bool
     , keyS        :: Bool
     , keyA        :: Bool
     , keyD        :: Bool
     , keyQ        :: Bool
     , keyE        :: Bool
     , keyZ        :: Bool
     , keyC        :: Bool
     , keyUp       :: Bool
     , keyDown     :: Bool
     , keyLeft     :: Bool
     , keyRight    :: Bool
     , keyPageUp   :: Bool
     , keyPageDown :: Bool
     , keyLShift   :: Bool
     , keyLCtrl    :: Bool
     , keyLAlt     :: Bool
     } deriving Show
