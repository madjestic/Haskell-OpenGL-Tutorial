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
-- The tooling around Vertex Array Objects
--
--------------------------------------------------------------------------------

module Graphics.RedViz.VAO
  (
    VAO
  , toVAO
  , VAO'
  , SVAO'
  , toVAO'
  , toVAO''
  ) where

import Data.Massiv.Array as A
import GHC.Float

import Graphics.RedViz.Utils

-- import Debug.Trace   as DT

type VAO = [[[Float]]]

toVAO
  :: [[Int]]
  -> [Float]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> [(Double, Double, Double)]
  -> VAO

toVAO idxs as cds ns ts ps = vaos
  where
    as'  = fmap (\a -> [a]) as                                :: [[Float]]
    cds' = fmap (\(r,g,b)   -> fmap double2Float [r,g,b]) cds :: [[Float]]
    ns'  = fmap (\(x,y,z)   -> fmap double2Float [x,y,z]) ns
    ts'  = fmap (\(u,v,w)   -> fmap double2Float [u,v,w]) ts
    ps'  = fmap (\(x,y,z)   -> fmap double2Float [x,y,z]) ps

    indices = fromLists' Par idxs :: (Array U Ix2 Int)
    as'' = fromLists' Par as'     :: (Array U Ix2 Float)
    cds''= fromLists' Par cds'    :: (Array U Ix2 Float)
    ns'' = fromLists' Par ns'     :: (Array U Ix2 Float)
    ts'' = fromLists' Par ts'     :: (Array U Ix2 Float)
    ps'' = fromLists' Par ps'     :: (Array U Ix2 Float)

    cList' = toLists2 . computeAs U $ concat' 1 [as'', cds'', ns'', ts'', ps''] :: [[Float]]
    
    ar = fromLists' Par cList' :: (Array U Ix2 Float)
    cListOpt =
      toLists2 . computeAs P <$>
      fmap (\row -> backpermute' (Sz (Prelude.length (idxs !! row) :. 13)) (\(i :. j) -> ((indices !> row) ! i) :. j) ar) [0 .. div (elemsCount indices) (elemsCount (indices !> 0))-1]
    --vaos = (DT.trace ("cListOpt" ++ show cListOpt) $ cListOpt)
    vaos = cListOpt

type VAO'  = [([Int], Int, [Float])]
type SVAO' =  ([Int], Int, [Float])

toVAO' :: [[Int]] -> [Int] -> [[Float]] -> VAO'
toVAO' is_ st_ vs_ = (,,) <$.> is_ <*.> st_ <*.> vs_

toVAO'' :: [Int] -> Int -> [Float] -> SVAO'
toVAO'' = (,,)

