{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Project.Model 
  ( Model (..)
  , path
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH


data Model
  =  Model
     {
       _path :: String
     } deriving Show

$(makeLenses ''Model)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Model
