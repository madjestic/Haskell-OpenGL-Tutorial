{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module NGL.Shape where

import Graphics.Rendering.OpenGL (Vertex4(..),                              
                                 TexCoord2(..),
                                 GLclampf(..))


data Shape = Square    Point   Side
           deriving Show
           
type VertexArray =[Vertex4 Float]
type UV          =[TexCoord2 Float]
type Point       =(Float, Float)
type Points      =[Point]
type Radius      = Float
type Side        = Float
type Divisions   = Int
type Texture     = String

type Drawable = ([Vertex4 Float],[TexCoord2 Float],String)

toDrawable :: Shape -> Drawable
toDrawable x = (vs, uv, tex)
           where
               vs'   = toPoints x               
               uv    = map toTexCoord2 vs'
               vs    = map toVertex4 $ vs'
               tex   = "test.png"
                      
toPoints :: Shape -> [Point]
toPoints (Square   pos side)     =  square pos side

toVertexArray :: [Point] -> VertexArray
toVertexArray xs = map toVertex4 xs

toVertex4 :: Point -> Vertex4 Float
toVertex4 p = (\(k,l) -> Vertex4 k l 0 1) p

toTextureCoord2 :: [Point] -> UV
toTextureCoord2 xs = map (\(k,l) -> TexCoord2 k l) xs

toTexCoord2 :: (a, a) -> TexCoord2 a
toTexCoord2 p = (\(k,l) -> TexCoord2 k l) p

data Projection = Planar                
                deriving Show

toUV :: Projection -> UV
toUV Planar = toTextureCoord2 ps
                  where ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
                             ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)]::Points

square :: Point -> Float -> [Point]
square pos side = [p1, p2, p3,
                   p1, p3, p4]
    where          
        x = fst pos
        y = snd pos
        r = side/2 
        p1 = (x + r, y + r)
        p2 = (x - r, y + r)
        p3 = (x - r, y - r)
        p4 = (x + r, y - r)
