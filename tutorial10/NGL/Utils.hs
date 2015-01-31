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
-- | Utils : utilities, helper functions
-- |
--------------------------------------------------------------------------------

module NGL.Utils where


-- | Group list into indevidual pairs: [1,2,3,4] => [(1,2),(3,4)]. 
--   Works only with even number of elements
pairs :: [t] -> [(t, t)]
pairs [] = []
pairs [x] = error "Non-even list for pair function"
pairs (x:y:xs) = (x,y):pairs xs

-- | Undo pairs function
fromPairs :: [(a, a)] -> [a]
fromPairs [] = []
fromPairs ((x,y):xs) = x:y:fromPairs xs

-- implement/bind delaunay somewhere here
