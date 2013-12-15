module Shape where

import Graphics.Rendering.OpenGL (Vertex2(..))

data Shape = Circle Pivot Radius
           | Square Pivot Radius
           | Triangle Vertex Vertex Vertex
           | Polygon [Vertex]
           deriving Show

type Vertex = Vertex2 Float
type Pivot = Vertex2 Float
type Radius = Float
type Graphics = [Vertex]

vertex :: Float -> Float -> Vertex
vertex x y = Vertex2 x y

triangle :: Vertex -> Vertex -> Vertex -> Shape
triangle = Triangle

toGraphics :: Shape -> [Vertex]
toGraphics (Triangle v1 v2 v3) = [v1, v2, v3]

v1 = vertex (-0.90) (-0.90)
v2 = Shape.vertex 0.85  (-0.90)
v3 = Shape.vertex (-0.90)   0.85

triangle' = triangle v1 v2 v3
