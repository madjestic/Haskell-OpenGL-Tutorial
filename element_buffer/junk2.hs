{-# LANGUAGE ExistentialQuantification #-}
--
-- An existential type encapsulating types that can be Shown
-- The interface to the type is held in the show method dictionary
--
-- Create your own typeclass for packing up other interfaces
--
data Showable = forall a . Show a => ToShowable a

--
-- And a nice existential builder
--
pack :: Show a => a -> Showable
pack = ToShowable

--
-- A heteoregenous list of Showable values
--
hlist :: [Showable]
hlist = [ pack 3
        , pack 'x'
        , pack pi
        , pack "string"
        , pack (Just ()) ]

--
-- The only thing we can do to Showable values is show them
--
main :: IO ()
main = print $ map f hlist
    where
        f (ToShowable a) = show a
