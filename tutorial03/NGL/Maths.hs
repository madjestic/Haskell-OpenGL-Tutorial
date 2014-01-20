--------------------------------------------------------------------------------
-- |
-- | Module      :  Data
-- | Copyright   :  (c) Vladimir Lopatin 2013
-- | License     :  BSD3
-- |
-- | Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- | Stability   :  experimental
-- | Portability :  untested
-- |
-- | NGL.Maths tiny maths library to backup NGL.Data
-- | basic shapes types should be of 2 kinds:
-- | Shapes  positioned by center
-- | Shapes' positioned by bottom-left corner--
--------------------------------------------------------------------------------

module NGL.Maths where


dist :: (Float, Float) -> (Float,Float) -> Float
dist (x1,y1) (x2,y2) = sqrt(x*x+y*y)
     where
      x = x1-x2
      y = y1-y2
