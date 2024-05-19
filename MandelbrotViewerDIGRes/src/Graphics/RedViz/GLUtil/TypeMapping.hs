--------------------------------------------------------------------------------
-- Copyright:           (c) 2012,2013 Anthony Cowley
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
-- |This module contains classes and functions to relate Haskell types
-- with OpenGL DataTypes (typically used to describe the values stored
-- in arrays) and VariableTypes (used as attributes and uniforms in
-- GLSL programs).
module Graphics.RedViz.GLUtil.TypeMapping where
import Data.Int
import Data.Word
import Foreign.Storable (Storable)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL
import Linear (V1, V2, V3, V4, M22, M33, M44)

-- | A mapping from Haskell types to values of 'VariableType'. This
-- defines how Haskell values may be mapped to values that may be
-- bound to GLSL variables.
class HasVariableType a where
  variableType :: a -> VariableType

instance HasVariableType Float where variableType _ = Float'
instance HasVariableType Int32 where variableType _ = Int'
instance HasVariableType Word32 where variableType _ = UnsignedInt'

instance HasVariableType (V1 GLfloat) where variableType _ = Float'
instance HasVariableType (V2 GLfloat) where variableType _ = FloatVec2
instance HasVariableType (V3 GLfloat) where variableType _ = FloatVec3
instance HasVariableType (V4 GLfloat) where variableType _ = FloatVec4

instance HasVariableType (V1 Int32) where variableType _ = Int'
instance HasVariableType (V2 Int32) where variableType _ = IntVec2
instance HasVariableType (V3 Int32) where variableType _ = IntVec3
instance HasVariableType (V4 Int32) where variableType _ = IntVec4

instance HasVariableType (V1 Word32) where variableType _ = UnsignedInt'
instance HasVariableType (V2 Word32) where variableType _ = UnsignedIntVec2
instance HasVariableType (V3 Word32) where variableType _ = UnsignedIntVec3
instance HasVariableType (V4 Word32) where variableType _ = UnsignedIntVec4

instance HasVariableType (M22 GLfloat) where variableType _ = FloatMat2
instance HasVariableType (M33 GLfloat) where variableType _ = FloatMat3
instance HasVariableType (M44 GLfloat) where variableType _ = FloatMat4

instance forall t. HasVariableType t => HasVariableType [t] where
  variableType _ = variableType (undefined::t)

-- | Maps each 'VariableType' to its corresponding
-- 'DataType'. Typically this indicates the element type of composite
-- variable types (e.g. @variableDataType FloatVec2 = Float@). Note
-- that this is a partial mapping as we are primarily supporting the
-- use of these types as inputs to GLSL programs where types such as
-- Bool are not supported.
variableDataType :: VariableType -> DataType
variableDataType Float' = GL.Float
variableDataType FloatVec2 = GL.Float
variableDataType FloatVec3 = GL.Float
variableDataType FloatVec4 = GL.Float
variableDataType Int' = GL.Int
variableDataType IntVec2 = GL.Int
variableDataType IntVec3 = GL.Int
variableDataType IntVec4 = GL.Int
variableDataType UnsignedInt' = GL.UnsignedInt
variableDataType UnsignedIntVec2 = GL.UnsignedInt
variableDataType UnsignedIntVec3 = GL.UnsignedInt
variableDataType UnsignedIntVec4 = GL.UnsignedInt
variableDataType FloatMat2 = GL.Float
variableDataType FloatMat3 = GL.Float
variableDataType FloatMat4 = GL.Float
variableDataType FloatMat2x3 = GL.Float
variableDataType FloatMat2x4 = GL.Float
variableDataType FloatMat3x2 = GL.Float
variableDataType FloatMat3x4 = GL.Float
variableDataType FloatMat4x2 = GL.Float
variableDataType FloatMat4x3 = GL.Float
variableDataType _ = error "Unsupported variable type!"

-- |Open mapping from Haskell types to OpenGL types.
class Storable a => HasGLType a where
  glType :: a -> DataType

instance HasGLType GLint where glType _ = GL.Int
instance HasGLType Word8 where glType _ = GL.UnsignedByte
instance HasGLType Word16 where glType _ = GL.UnsignedShort
instance HasGLType Word32 where glType _ = GL.UnsignedInt
instance HasGLType Float where glType _ = GL.Float
