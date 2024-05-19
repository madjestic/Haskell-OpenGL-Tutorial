--------------------------------------------------------------------------------
-- |
-- Module      :  Descriptor
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A basic structure for passing to graphics driver.
--
--------------------------------------------------------------------------------

module Graphics.RedViz.Descriptor
  (Descriptor (..)) where

import Graphics.Rendering.OpenGL (VertexArrayObject, NumArrayIndices)

data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices
  deriving Show
