{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.RedViz.Widget
  ( Alignment (..)
  , Format (..)
  , Widget (..)
  , BBox (..)
  , xres
  , yres
  , xoffset
  , yoffset
  , alignment
  , soffset
  , zoffset
  , ssize
  , text
  , lable
  , format
  , bbox
  , rover
  , pressed
  , isPressed
  --, coords
  , defaultFormat
  , defaultCursorFormat
  ) where

import Control.Lens hiding (Empty)
import Data.Aeson
import GHC.Generics
--import Data.Aeson.TH
--import Data.Aeson.Encode.Pretty

import Graphics.RedViz.Backend

data Alignment =
   TL |TC |TR
  |CL |CC |CR
  |BL |BC |BR
  deriving (Generic, Show)
instance ToJSON Alignment
instance FromJSON Alignment

data Format -- move to Format.hs?
  =  Format
     { _alignment :: Alignment
     , _xres      :: Int
     , _yres      :: Int
     , _xoffset   :: Double
     , _yoffset   :: Double
     , _zoffset   :: Double
     , _soffset   :: Double -- scale Offset
     , _ssize     :: Double -- scale Size
     } deriving (Generic, Show)
$(makeLenses ''Format)
instance ToJSON Format
instance FromJSON Format

defaultFormat :: Format
defaultFormat = Format CC 1280 720 0.0 0.0 0.0 0.085 1.0

defaultCursorFormat :: Int -> Int -> Format
defaultCursorFormat resx' resy' =
  Format CC resx' resy' 0.0 0.0 0.0 0.0 0.2
  
data BBox
  =  BBox
     { -- TL BR
       _bbx0 :: Double
     , _bby0 :: Double
     , _bbx1 :: Double
     , _bby1 :: Double
     } deriving (Generic, Show)
$(makeLenses ''BBox)
instance ToJSON BBox
instance FromJSON BBox

data Widget
  =  Empty
  |  TextField
     { _active  :: Bool
     , _text    :: [String]
     , _format  :: Format
     , _options :: BackendOptions
     }
  |  FPS
     { _active  :: Bool
     , _format  :: Format
     , _options :: BackendOptions
     }
  |  Button -- True when pressed, False when released
     { _active  :: Bool
     , _lable   :: String
     , _bbox    :: BBox
     , _rover   :: Bool
     , _pressed :: Bool
     , _format  :: Format
     , _options :: BackendOptions     
     }
  |  Toggle -- same as Buttong, but stays True, untill pressed again
     { _active  :: Bool
     , _lable   :: String
     , _bbox    :: BBox
     , _over    :: Bool
     , _pressed :: Bool
     , _format  :: Format
     , _options :: BackendOptions     
     }
  |  MultiToggle
     { _active  :: Bool
     , _current :: Int
     , _toggles :: [Widget]
     , _options :: BackendOptions     
     }
  |  Icon
     { _active  :: Bool
     , _lable   :: String -- to show tooltips
     , _idx     :: Int    -- Model index
     , _format  :: Format
     , _options :: BackendOptions
     }
  |  Cursor
     { _active  :: Bool
     , _lable   :: String -- to show tooltips
     , _format  :: Format
     , _options :: BackendOptions
     } deriving (Generic)
$(makeLenses ''Widget)
--deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Widget

instance ToJSON Widget
instance FromJSON Widget

instance Show Widget where
  show (TextField b t f opts)     = show ("TextField :"  :: String) ++ show (b, t, f, opts)
  show (FPS b f opts)             = show ("FPS :"        :: String) ++ show (b, f, opts)
  show (Button a l bb o p f opts) = show ("Button :"     :: String) ++ show (a, l, bb, o, p, f, opts)
  show (Icon   a l i f opts)      = show ("Icon :"       :: String) ++ show (a, l, i, f, opts)
  show (Cursor a l f opts)        = show ("Cursor :"     :: String) ++ show (a, f, l, opts)
  show (Toggle a l bb o p f opts) = show ("Toggle :"     :: String) ++ show (a, l, bb, o, p, f, opts)
  show (MultiToggle a c ts opts)  = show ("MultiToggle :":: String) ++ show (a, c, ts, opts)
  show (Empty)                    = show ("Empty Widget" :: String)

instance Semigroup Widget where
  x <> _ = x

isPressed :: Maybe Widget -> Bool
isPressed wgt = 
  case wgt of
    Just w  -> _pressed w
    Nothing -> False
