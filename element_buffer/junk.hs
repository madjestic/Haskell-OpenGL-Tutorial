{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Main where

import Data.Dynamic

foo = [1,2,3]

data T
     = ConsInt    Int
     | ConsString String
     | ConsChar   Char
     deriving Show

hlist :: [Dynamic]
hlist = [ toDyn "string"
        , toDyn (7 :: Int)
        , toDyn 'x'
        ]

data Showable
     where
     ToShowable :: Show a => a -> Showable


-- data Showable = forall a . Show a => ToShowable a

hlist' :: [Showable]
hlist' = [ pack "string"
         , pack (7 :: Int)
         , pack 'x'
         , pack (Just ())
         ]

pack :: Show a => a -> Showable
pack = ToShowable

main :: IO ()
main = print $ map f hlist'
    where
        f (ToShowable a) = show a
