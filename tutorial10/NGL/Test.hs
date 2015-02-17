module Test where

import Graphics.Rendering.OpenGL (Vertex4(..),
                                  Color4(..),
                                  TexCoord4(..),
                                  TexCoord2(..),
                                  GLclampf(..))
import NGL.Utils
import TinyMath.TinyMath


data Shape = Circle    Point   Radius Divisions
           | Square    Point   Side
           | Rect      Point   Point
           | Line      Point   Point  Float  -- | Ordered pair to store directionality
           | Triangle  Point   Point  Point
           | Quad     [Point]    -- | BL toVertex4 TR toVertex4
           | Polygon  [Point]    -- | [Triangle] ? 
           | Polyline [Point]  Float
           | Curve    [Point]
           deriving Show


data Color = Red
            | Green
            | Blue
            | White
            | Black
            | RGB    GLclampf GLclampf GLclampf 
            | RGBA   GLclampf GLclampf GLclampf GLclampf
            | Default
            deriving Show

instance Eq Color where
    Red          == Red   = True
    Green        == Green = True
    Blue         == Blue  = True
    White        == White = True
    Black        == Black = True
    RGB _ _ _    == RGB _ _ _    = True
    RGBA _ _ _ _ == RGBA _ _ _ _ = True
    Default      == Default      = True
    _ == _ = False


data Transform = Rotate2D Float Point 
               | Translate2D Point Point
               deriving Show


-- data Drawable  = Shape
--                | Picture
--                deriving Show
-- error : double type declaration: Shape delcared 2 times

data Picture = Shape Texture

class Drawable a where
instance Drawable Shape where
instance Drawable Picture where


type VertexArray =[Vertex4 Float]
type UV          =[TexCoord2 Float]
type Points      =[Point]
type Point       = Point2D
type Radius      = Float
type Side        = Float
type Divisions   = Int
type Texture   = String

-- draw :: Drawable -> IO ()
-- draw = undefined

toPicture :: Shape -> UV -> Texture -> Picture
toPicture = undefined

