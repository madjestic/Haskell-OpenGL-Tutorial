{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.RedViz.Backend where

--import Control.Lens hiding (Empty, (.=))
import Data.Aeson
import GHC.Generics

-- import Data.Aeson (Options(fieldLabelModifier))
-- import Data.Aeson.TH

import Graphics.Rendering.OpenGL
--import Graphics.Rendering.OpenGL.GL.Capability (Capability(..))
--import Graphics.RedViz.Primitives (Primitive(PolyLine))

data Backend
  = OpenGL
  | Vulkan

data BackendOptions
  =  BackendOptions
     {
       primitiveMode :: PrimitiveMode -- Triangles | Points
     , bgrColor      :: Color4 GLfloat
     , ptSize        :: Float
     , depthMsk      :: Capability
     } deriving (Generic, Show)
--deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''BackendOptions
-- instance ToJSON Capability where
--   toJSON = show a
instance FromJSON BackendOptions

instance ToJSON Capability where
  toJSON c = case c of
    Enabled  -> "Enabled"
    Disabled -> "Disabled"

instance FromJSON Capability where
  parseJSON (String o) =
    case o of
      "Enabled"  -> return Enabled
      "Disabled" -> return Disabled
      _ -> error $ "Invalid Capability: " ++ show o
  parseJSON _ = error "Json format not exptected"

instance ToJSON BackendOptions

instance ToJSON PrimitiveMode where
  toJSON p = case p of
    Points        -> "Points"
    Lines         -> "Lines"
    LineLoop      -> "LineLoop"
    LineStrip     -> "LineStrip"
    Triangles     -> "Triangles"
    TriangleStrip -> "TriangleStrip"
    TriangleFan   -> "TriangleFan"
    Quads         -> "Quads" 
    QuadStrip     -> "QuadStrip"
    Polygon       -> "Polygon"
    Patches       -> "Patches"

instance FromJSON PrimitiveMode where
  parseJSON (String o) =
    case o of
      "Points"        -> return Points
      "Lines"         -> return Lines
      "LineLoop"      -> return LineLoop
      "LineStrip"     -> return LineStrip
      "Triangles"     -> return Triangles
      "TriangleStrip" -> return TriangleStrip
      "TriangleFan"   -> return TriangleFan
      "Quads"         -> return Quads
      "QuadStrip"     -> return QuadStrip
      "Polygon"       -> return Polygon
      "Patches"       -> return Patches
      _           -> error $ "Invalid PrimitiveMode: " ++ show o
  parseJSON _ = error "Json format not exptected"

instance ToJSON (Color4 GLfloat) where
  toJSON (Color4 r g b a) =
    object ["red" .= r, "green" .= g, "blue" .= b, "alpha" .= a]

instance FromJSON (Color4 GLfloat) where
  parseJSON (Object o) =
    do
      red   <- o .: "red"
      green <- o .: "green"
      blue  <- o .: "blue"
      alpha <- o .: "alpha"
      return $ Color4 red green blue alpha
  parseJSON _ = error "Json format not exptected"

defaultBackendOptions :: BackendOptions
defaultBackendOptions = defOpts

defaultBackendOptions' :: BackendOptions
defaultBackendOptions' = defOpts'
  
defOpts :: BackendOptions
defOpts =
      BackendOptions
      { primitiveMode = Triangles
      , bgrColor      = Color4 0.0 0.0 0.0 1.0
      , ptSize        = 1.0
      , depthMsk      = Enabled
      }

defOpts' :: BackendOptions
defOpts' =
      BackendOptions
      { primitiveMode = Lines
      , bgrColor      = Color4 0.0 0.0 0.0 1.0
      , ptSize        = 1.0
      , depthMsk      = Enabled
      }     
