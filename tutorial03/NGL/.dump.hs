module Dump where

foo :: Float -> Maybe Float -> Float
foo x Nothing = x
foo _ (Just y) = y
