--------------------------------------------------------------------------------
-- |
-- Module      :  Rendering
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling OpenGL buffers and rendering.
--
--------------------------------------------------------------------------------

module Graphics.RedViz.GLUtil
  ( readTexture
  , texture2DWrap
  ) where

import Graphics.RedViz.GLUtil.JuicyTextures (readTexture)
import Graphics.RedViz.GLUtil.Textures      (texture2DWrap)
