--------------------------------------------------------------------------------
-- Copyright:           (c) 2012,2013 Anthony Cowley
--------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, 
             ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
-- |Utilities for loading texture data.
module Graphics.RedViz.GLUtil.Textures where

import Control.Monad (forM_)
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as GL
import Data.Array.Storable (StorableArray, withStorableArray)
import Data.ByteString.Internal (ByteString, toForeignPtr)
import Data.Vector.Storable (Vector, unsafeWith)
import Data.Word (Word8, Word16)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, castPtr, nullPtr)
import Foreign.Marshal.Array (withArray)

import Graphics.RedViz.GLUtil.TypeMapping (HasGLType(..))

-- |Pixel format of image data.
data TexColor = TexMono | TexRG | TexRGB | TexBGR | TexRGBA

-- |A basic texture information record.
data TexInfo a = TexInfo { texWidth  :: GLsizei
                         , texHeight :: GLsizei
                         , texColor  :: TexColor
                         , texData   :: a }

-- |Helper for constructing a 'TexInfo' using Haskell 'Int's for image
-- dimensions.
texInfo :: Int -> Int -> TexColor -> a -> TexInfo a
texInfo w h = TexInfo (fromIntegral w) (fromIntegral h)

-- |Class for containers of texture data.
class HasGLType (Elem a) => IsPixelData a where
  type Elem a
  withPixels :: a -> (Ptr (Elem a) -> IO c) -> IO c

instance HasGLType b => IsPixelData [b] where
  type Elem [b] = b
  withPixels = withArray

instance HasGLType b => IsPixelData (Ptr b) where
  type Elem (Ptr b) = b
  withPixels = flip ($)

instance HasGLType b => IsPixelData (ForeignPtr b) where
  type Elem (ForeignPtr b) = b
  withPixels = withForeignPtr

instance HasGLType b => IsPixelData (StorableArray i b) where
  type Elem (StorableArray i b) = b
  withPixels = withStorableArray

instance HasGLType b => IsPixelData (Vector b) where
  type Elem (Vector b) = b
  withPixels = unsafeWith

instance IsPixelData ByteString where
  type Elem ByteString = Word8
  withPixels b m = aux . toForeignPtr $ b
    where aux (fp,o,_) = withForeignPtr fp $ \p ->
                           m (plusPtr p o)

-- |Wrapper whose 'IsPixelData' instance treats the pointer underlying
-- a 'ByteString' as an array of 'Word16's.
newtype ShortString = ShortString ByteString

instance IsPixelData ShortString where
  type Elem ShortString = Word16
  withPixels (ShortString b) m = aux. toForeignPtr $ b
    where aux (fp,o,_) = withForeignPtr fp $ \p ->
                           m (plusPtr (castPtr p :: Ptr Word16) o)

-- |Create a new 2D texture with uninitialized contents.
freshTexture :: forall a proxy. HasGLType a
             => Int -> Int -> TexColor -> proxy a -> IO TextureObject
freshTexture w h c _ = loadTexture $ texInfo w h c (nullPtr::Ptr a)

-- |Create a new 2D texture with uninitialized 'Word8' contents.
freshTextureWord8 :: Int -> Int -> TexColor -> IO TextureObject
freshTextureWord8 w h c = loadTexture $ texInfo w h c (nullPtr::Ptr Word8)

-- |Create a new 2D texture with uninitialized 'GLfloat' contents.
freshTextureFloat :: Int -> Int -> TexColor -> IO TextureObject
freshTextureFloat w h c = loadTexture $ texInfo w h c (nullPtr::Ptr GLfloat)

-- |Create a new 2D texture with data from a 'TexInfo'.
loadTexture :: IsPixelData a => TexInfo a -> IO TextureObject
loadTexture tex = do [obj] <- genObjectNames 1
                     reloadTexture obj tex
                     return obj

-- |Replace a 2D texture's pixel data with data from a 'TexInfo'.
reloadTexture :: forall a. IsPixelData a => 
                 TextureObject -> TexInfo a -> IO ()
reloadTexture obj tex = do textureBinding Texture2D $= Just obj
                           loadTex $ texColor tex
  where loadTex TexMono = case pixelType of
                            GL.UnsignedShort -> loadAux Luminance16 Luminance
                            GL.Float         -> loadAux R32F Red
                            GL.HalfFloat     -> loadAux R16F Red
                            GL.UnsignedByte  -> loadAux R8 Red
                            _                -> loadAux Luminance' Luminance
        loadTex TexRG = case pixelType of
                          GL.UnsignedShort -> loadAux RG16 RGInteger
                          GL.Float -> loadAux RG32F RG
                          GL.HalfFloat -> loadAux RG16F RG
                          GL.UnsignedByte -> loadAux RG8UI RGInteger
                          GL.Byte -> loadAux RG8I RGInteger
                          GL.Int -> loadAux RG32I RGInteger
                          GL.UnsignedInt -> loadAux RG32UI RGInteger
                          _ -> error "Unknown pixelType for TexRG"
        loadTex TexRGB = loadAux RGBA' RGB
        loadTex TexBGR = loadAux RGBA' BGR
        loadTex TexRGBA = case pixelType of
                            GL.UnsignedShort -> loadAux RGBA16 RGBA
                            GL.Float -> loadAux RGBA32F RGBA
                            GL.HalfFloat -> loadAux RGBA16F RGBA
                            GL.Int -> loadAux RGBA32I RGBAInteger
                            GL.UnsignedInt -> loadAux RGBA32UI RGBAInteger
                            _ -> loadAux RGBA' RGBA
        sz = TextureSize2D (texWidth tex) (texHeight tex)
        pixelType = glType (undefined::Elem a)
        loadAux i e = withPixels (texData tex) $ 
                      (texImage2D Texture2D NoProxy 0 i sz 0 .
                       PixelData e pixelType)

-- | Set texture coordinate wrapping options for both the 'S' and 'T'
-- dimensions of a 2D texture.
texture2DWrap :: StateVar (Repetition, Clamping)
texture2DWrap = makeStateVar (get (textureWrapMode Texture2D S))
                             (forM_ [S,T] . aux)
  where aux x d = textureWrapMode Texture2D d $= x

-- | Set texture coordinate wrapping options for the 'S', 'T', and 'R'
-- dimensions of a 3D texture.
texture3DWrap :: StateVar (Repetition, Clamping)
texture3DWrap = makeStateVar (get (textureWrapMode Texture2D S))
                             (forM_ [S,T,R] . aux)
  where aux x d = textureWrapMode Texture2D d $= x


-- | Bind each of the given textures to successive texture units at
-- the given 'TextureTarget' starting with texture unit 0.
withTextures :: BindableTextureTarget t => t -> [TextureObject] -> IO a -> IO a
withTextures tt ts m = do mapM_ aux (zip ts [0..])
                          r <- m
                          cleanup 0 ts
                          activeTexture $= TextureUnit 0
                          return r
  where aux (t,i) = do activeTexture $= TextureUnit i
                       textureBinding tt $= Just t
        cleanup _ [] = return ()
        cleanup i (_:ts') = do activeTexture $= TextureUnit i
                               textureBinding tt $= Nothing
                               cleanup (i+1) ts'

-- | Bind each of the given 2D textures to successive texture units
-- starting with texture unit 0.
withTextures2D :: [TextureObject] -> IO a -> IO a
withTextures2D = withTextures Texture2D

-- | Bind each of the given textures to the texture unit they are
-- paired with. The given action is run with these bindings, then the
-- texture bindings are reset. If you don't care which texture units
-- are used, consider using 'withTextures' or 'withTextures2D'.
withTexturesAt :: BindableTextureTarget t
               => t -> [(TextureObject,GLuint)] -> IO a -> IO a
withTexturesAt tt ts m = do mapM_ aux ts
                            r <- m
                            mapM_ (cleanup . snd) ts
                            return r
  where aux (t,i) = do activeTexture $= TextureUnit i
                       textureBinding tt $= Just t
        cleanup i = do activeTexture $= TextureUnit i
                       textureBinding tt $= Nothing
