module Main where

import HNGL.Data
import HNGL.Rendering


display :: [Instanceable] -> IO ()
display xs = draw $ concat $ map toDrawable xs

main :: IO ()
main = do
    display [(Square (-0.5, -0.5) 1.0),(Circle (0.5, 0.5) 0.5 100)]
