{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Object
  ( Object' (..)
  , descriptors
  , materials
  , programs
  , transforms
  , transform0
  , transform1
  , ypr0
  , ypr
  , time
  , options
  , defaultObject'
  , name
  ) where

import Control.Lens hiding (transform, pre)
import Graphics.Rendering.OpenGL (Program)
import Linear.Matrix
import Linear.V3

import Graphics.RedViz.Descriptor
import Graphics.RedViz.Material hiding (name, _name)
import Graphics.RedViz.Backend

data Object'
  =  Object'
     {
        _name        :: String
      , _descriptors :: [Descriptor] -- | Material is bound in Descriptor, but we also use this data for draw-call separation per material.
       -- data Descriptor =
       -- Descriptor VertexArrayObject NumArrayIndices
      , _materials   :: [Material]    -- | hence [Material] is present on the Object level too, we use that value, instead of looking it up from respective VGeo.
      , _programs    :: [Program]     -- | Shader Programs
      , _transforms  :: ![M44 Double] -- | transforms for parts (object fragments)
      , _transform0  :: !(M44 Double) -- | initial basis (position/orientation in world space)
      , _transform1  :: !(M44 Double) -- | basis (position/orientation in world space)
      , _ypr0        :: !(V3 Double)
      , _ypr         :: !(V3 Double)
      , _time        :: Double
      , _options     :: BackendOptions
     } deriving Show
$(makeLenses ''Object')

zeroV3 :: V3 Double
zeroV3 = V3 0 0 0

-- defaultObject' :: Object'
-- defaultObject' = Object' [] [] [] [] (identity::M44 Double) zeroV3 zeroV3 0.0
defaultObject' :: Object'
defaultObject' =
  Object'
  {
    _descriptors = []
  , _materials   = []
  , _programs    = []
  , _transforms  = []
  , _transform0  = identity :: M44 Double
  , _transform1  = identity :: M44 Double
  , _ypr0        = zeroV3
  , _ypr         = zeroV3
  , _time        = 0.0
  , _options     = defaultBackendOptions
  , _name        = "defaultObject'"
  }

