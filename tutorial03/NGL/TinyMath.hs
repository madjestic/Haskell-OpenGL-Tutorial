--------------------------------------------------------------------------------
-- |
-- | Module      :  Data
-- | Copyright   :  (c) Vladimir Lopatin 2014
-- | License     :  BSD3
-- |
-- | Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- | Stability   :  experimental
-- | Portability :  untested
-- |
-- | TinyMath library - a set of basic mathematical functions from:
-- | trigonometry
-- |
-- | basic shapes types should be of 2 kinds:
-- | Shapes  positioned by center
-- | Shapes' positioned by bottom-left corner--
--------------------------------------------------------------------------------

module NGL.TinyMath where

type Matrix2D = (Float, Float,
                 Float, Float)


-- | converts degrees to radians
toRadians :: Float -> Float
toRadians x = x*pi/180


-- | converts radians to degrees
fromRadians :: Float -> Float
fromRadians x = x/pi*180

rotate2D' :: Float -> [(Float, Float)] -> [(Float, Float)]
rotate2D' a = map (rotate2D a)

rotate2D :: Float -> (Float, Float) -> (Float, Float)
rotate2D theta (x,y) = (x',y')
         where
            x' = x * cos theta - y * sin theta
            y' = x * sin theta + y * cos theta


normalize :: (Float, Float) -> (Float, Float)
normalize v@(x,y) = (x*len', y*len')
          where
            len' = 1.0/len v

len :: (Float, Float) -> Float
len (x,y) = sqrt(x*x+y*y)


-- | multiply matrix by vector
mulMatrVect :: Matrix2D -> (Float, Float) -> (Float, Float)
mulMatrVect (x1,x2,y1,y2) (x,y) = ((x1+x2)*x,(y1+y2)*y)


addVect :: (Float, Float) -> (Float, Float) -> (Float, Float)
addVect (x1,y1) (x2,y2) = (x1+x2, y1+y2)
