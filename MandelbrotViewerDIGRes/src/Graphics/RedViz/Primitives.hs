--------------------------------------------------------------------------------
-- |
-- Module      :  Primitives
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Primitive drawable structures.
--
--------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Primitives where

--import Linear.V3       as LV3

data Primitive =
  Curve
  deriving Show

-- polyline :: [V3 Double] -> Float -> [V3 Double]
-- polyline ps w = concatMap (\(x,y) -> line x y w) $ pairs $ abbcca ps

-- line :: V3 Double -> V3 Double -> Float -> [V3 Double]
-- line (x1,y1) (x2,y2) w = map (addVectors (x1,y1)) $ rotate2D' theta $ rect (0.0,-w/2) (len,w/2) -- rotation is wrong
--      where 
--            (x,y) = normalize $ ((x2-x1),(y2-y1))
--            theta = signum y * acos x                               -- | angle in radians
--            len   = sqrt((x2-x1)^2+ (y2-y1)^2)

-- abbcca :: [a] -> [a]
-- abbcca (x:xs) = [x] ++ (concat $ map (\(x,y) -> [x,y]) $ map (\x -> (x, x)) (init xs)) ++ [last xs]
           
