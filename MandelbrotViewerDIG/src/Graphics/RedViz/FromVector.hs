module Graphics.RedViz.FromVector
  ( FromVector(..)
  ) where

import Graphics.Rendering.OpenGL as GL        hiding (Size, Position, Point, position)

class FromVector a where
  toVertex4  :: a -> Vertex4 Double      
