module Texture
   (
    loadGLTextureFromFile,

   ) where

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Codec.Picture
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Codec.Picture as Pic

loadGLTextureFromFile :: FilePath -> IO GL.TextureObject
loadGLTextureFromFile f = do t <- either error id <$> readTexture f
                             textureFilter Texture2D $= ((Linear', Nothing), Linear')
                             texture2DWrap $= (Mirrored, ClampToEdge)
                             return t
