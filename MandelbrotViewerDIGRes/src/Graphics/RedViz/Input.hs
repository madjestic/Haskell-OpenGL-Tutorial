--------------------------------------------------------------------------------
-- |
-- Module      :  RedViz
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The user input interface layer.
--
--------------------------------------------------------------------------------

{-# LANGUAGE CPP    #-}

module Graphics.RedViz.Input
  (
    module Graphics.RedViz.Input.FRP.Yampa
  , module Graphics.RedViz.Input.Mouse
  , module Graphics.RedViz.Input.Keyboard

  ) where

import Graphics.RedViz.Input.Mouse
import Graphics.RedViz.Input.Keyboard
import Graphics.RedViz.Input.FRP.Yampa
