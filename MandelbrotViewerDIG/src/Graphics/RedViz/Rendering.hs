--------------------------------------------------------------------------------
-- |
-- Module      :  Rendering
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling OpenGL buffers and rendering.
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP    #-}

module Graphics.RedViz.Rendering
  ( openWindow
  , closeWindow
  , render
  , renderString
  , renderIcons
  , renderIcon
  , renderWidget
  , renderCursor
  , toDescriptor
  , initVAO
  , bindUniforms
  , bindTexture
  , bindTextureObject
  , loadTex
  , Backend (..)
  , BackendOptions (..)
  ) where

import Control.Monad
import Data.Maybe                             (fromMaybe)
import Data.Text                              (Text)
import Data.UUID
import Data.List.Split                        (splitOn)
import Foreign.C
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr)
import Foreign.Storable                       (sizeOf)
import Graphics.Rendering.OpenGL as GL hiding (color, normal, Size)
import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot, project, Texture)
import Linear.Vector
import Data.Foldable             as DF        (toList)
import Linear.Projection         as LP        (infinitePerspective)
import Unsafe.Coerce
import Control.Lens                    hiding (indexed)
import Graphics.RedViz.GLUtil                 (readTexture, texture2DWrap)
import GHC.Float                              (int2Double, double2Float)

import Graphics.RedViz.LoadShaders
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Texture           as T
import Graphics.RedViz.Drawable
import Graphics.RedViz.VAO (SVAO')
import Graphics.RedViz.Widget (Widget (..), Format (..), xoffset, yoffset, zoffset, alignment, Alignment(..), soffset, ssize, xres, yres)
import Graphics.RedViz.Backend

--import Debug.Trace as DT

debug :: Bool
#ifdef DEBUGSHADERS
debug = True
#else
debug = False
#endif

openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) =
  do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality
       when (renderQuality /= SDL.ScaleLinear) $
         putStrLn "Warning: Linear texture filtering not enabled!"

    let config = OpenGLConfig { glColorPrecision = V4 8 8 8 0
                              , glDepthPrecision = 24
                              , glStencilPrecision = 8
                              , glMultisampleSamples = 4
                              , glProfile = Core Normal 4 5
                              }

    depthFunc $= Just Less

    window <- SDL.createWindow
              title
              SDL.defaultWindow
              { SDL.windowInitialSize = V2 sizex sizey
              , SDL.windowGraphicsContext = OpenGLContext config
              }      

    SDL.showWindow window
    _ <- SDL.glCreateContext window

    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window =
  do
    SDL.destroyWindow window
    SDL.quit

renderString :: (Drawable -> IO ()) -> [Drawable] -> Format -> String -> IO ()
renderString cmds fntsDrs fmt str =
  mapM_ cmds $ format fmt $ drawableString fntsDrs str

renderIcon :: (Drawable -> IO ()) -> [Drawable] -> Format -> Int -> IO ()
renderIcon cmds icnsDrs fmt idx =
  --cmds $ formatting fmt $ (drawableIcon icnsDrs idx, 0)
  cmds $ formatting fmt (drawableIcon icnsDrs idx, 0)

renderWidget :: Double -> [Drawable] -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderWidget dt' drs drs' cmds wgt =
  case wgt of
    Icon a _ idx fmt _ ->
      when a $ do
      renderIcon cmds drs' fmt idx --"icon"
    Cursor a _ fmt _ ->
      when a $ do
      renderCursor cmds drs' fmt 0 --"cursor"
    TextField a t f _ ->
      when a $ renderString cmds drs f $ concat t
    Button a l _ _ _ f _ ->
      when a $ renderString cmds drs f l
    FPS a f _ ->
      when a $ do
        renderString cmds drs f $ "fps:" ++ show (round (1.0/dt') :: Integer)
    _ -> return ()

renderCursor :: (Drawable -> IO ()) -> [Drawable] -> Format -> Int -> IO ()
renderCursor cmds icnsDrs fmt idx =
  cmds $ formatCursor fmt $ drawableIcon icnsDrs idx

renderIcons :: (Drawable -> IO ()) -> [Drawable] -> Format -> [Int] -> IO ()
renderIcons cmds icnsDrs fmt idxs =
  mapM_ cmds $ format fmt $ drawableIcons icnsDrs idxs

-- | given a string of drawables, return a formatted string (e.g. add offsets for drawable chars)
format :: Format -> [Drawable] -> [Drawable]
format fmt drs = drw
  where
    drw = fmap (formatting fmt) (zip drs [0..])

formatting :: Format -> (Drawable, Int) -> Drawable
formatting fmt (drw, offset) = drw'
  where
    rot0 = view _m33 (view (uniforms . u_xform) drw)
    tr0  = view translation (view (uniforms . u_xform) drw)
    s1   = fmt ^. soffset
    s2   = identity !!* fmt ^. ssize :: V3 (V3 Double)
    (x, y) =
      case fmt ^. alignment of
        TL -> (-1.0, 1.0)
        TC -> ( 0.0, 1.0)
        TR -> ( 1.0, 1.0)
        CL -> (-1.0, 0.0)
        CC -> ( 0.0, 0.0)
        CR -> ( 1.0, 0.0)
        BL -> (-1.0,-1.0)
        BC -> ( 0.0,-1.0)
        BR -> ( 1.0, 1.0)
    x'    = fmt ^. xoffset
    y'    = fmt ^. yoffset
    z'    = fmt ^. zoffset
    offsetM44 =
      mkTransformationMat
      (rot0 * s2)
      (tr0 ^+^ V3 ((x + x') + fromIntegral offset*s1) (y + y') z')
      --(tr0 ^+^ V3 0 0 0.2)
    drw' = set (uniforms . u_xform) offsetM44 drw

formatCursor :: Format -> Drawable -> Drawable
formatCursor fmt drw = drw'
  where
    rot0 = view _m33 (view (uniforms . u_xform) drw)
    tr0  = view translation (view (uniforms . u_xform) drw)
    s    = identity !!* fmt ^. ssize :: V3 (V3 Double)-- fmt ^. ssize   -- 1.0    -- s Size
    (x, y) = (fmt ^. xoffset, fmt ^. yoffset)
    (resx, resy) = (int2Double $ fmt ^. xres, int2Double $ fmt ^. yres)
    
    offsetM44 =
      mkTransformationMat
      (rot0 * s)
      (tr0 ^+^ V3 (x/resx-0.5) (0.5-y/resy) 0.0)
    drw' = set (uniforms . u_xform) offsetM44 drw
      -- (DT.trace (
      --     "x, y : " ++ show (x,y) ++ "\n" ++
      --     "x/resx, y/resy : " ++ show (x/resx,y/resy) ++ "\n" ) drw)

-- | Alphabet of drawables -> String -> String of drawables
drawableString :: [Drawable] -> String -> [Drawable]
drawableString drs str = drws
  where
    drws = fmap (drawableChar drs) str

drawableIcons :: [Drawable] -> [Int] -> [Drawable]
drawableIcons  drs idxs = drws
  where
    drws = (drs!!) <$> idxs

drawableIcon :: [Drawable] -> Int -> Drawable
drawableIcon drs idx = drs!!idx

-- | Alphabet of drawables -> Char -> a drawable char
drawableChar :: [Drawable] -> Char -> Drawable
drawableChar drs chr =
  case chr of
    '0' -> head drs -- TODO: replace with hash lookup
    '1' -> drs!!1
    '2' -> drs!!2
    '3' -> drs!!3
    '4' -> drs!!4
    '5' -> drs!!5
    '6' -> drs!!6
    '7' -> drs!!7
    '8' -> drs!!8
    '9' -> drs!!9
    'a' -> drs!!10
    'b' -> drs!!11
    'c' -> drs!!12
    'd' -> drs!!13
    'e' -> drs!!14
    'f' -> drs!!15
    'g' -> drs!!16
    'h' -> drs!!17
    'i' -> drs!!18
    'j' -> drs!!19
    'k' -> drs!!20
    'l' -> drs!!21
    'm' -> drs!!22
    'n' -> drs!!23
    'o' -> drs!!24
    'p' -> drs!!25
    'q' -> drs!!26
    'r' -> drs!!27
    's' -> drs!!28
    't' -> drs!!29
    'u' -> drs!!30
    'v' -> drs!!31
    'w' -> drs!!32
    'x' -> drs!!33
    'y' -> drs!!34
    'z' -> drs!!35
    '+' -> drs!!36
    '-' -> drs!!37
    '=' -> drs!!38
    '>' -> drs!!39
    ',' -> drs!!40
    '.' -> drs!!41
    '?' -> drs!!42
    '!' -> drs!!43
    ' ' -> drs!!44
    '*' -> drs!!45
    '/' -> drs!!46
    ':' -> drs!!47
    '\''-> drs!!48
    'A' -> drs!!49
    'B' -> drs!!50
    'C' -> drs!!51
    'D' -> drs!!52
    'E' -> drs!!53
    'F' -> drs!!54
    'G' -> drs!!55
    'H' -> drs!!56
    'I' -> drs!!57
    'J' -> drs!!58
    'K' -> drs!!59
    'L' -> drs!!60
    'M' -> drs!!61
    'N' -> drs!!62
    'O' -> drs!!63
    'P' -> drs!!64
    'Q' -> drs!!65
    'R' -> drs!!66
    'S' -> drs!!67
    'T' -> drs!!68
    'U' -> drs!!69
    'V' -> drs!!70
    'W' -> drs!!71
    'X' -> drs!!72
    'Y' -> drs!!73
    'Z' -> drs!!74
    '<' -> drs!!75
    _   -> error "font drs index out of range             "

type MousePos    = (Double, Double)    

render :: [Texture] -> [(UUID, GLuint)] ->  MousePos -> Drawable -> IO ()
render txs hmap mpos (Drawable _ unis (Descriptor vao' numIndices') prog opts) =
  do
 -- print $ "render.name : " ++ name
 -- print $ "render.unis :" ++ show unis ++ "\n render.txs :" ++ show txs ++ "\n render.hmap : " ++ show hmap
    bindUniforms txs unis prog mpos hmap
    bindVertexArrayObject $= Just vao'

    GL.pointSize $= ptSize opts --0.001
  --GL.pointSmooth $= Enabled
    GL.depthMask $= depthMsk opts

    drawElements (primitiveMode opts) numIndices' GL.UnsignedInt nullPtr

    cullFace  $= Just Back
    depthFunc $= Just Less

bindTextureObject :: GLuint -> TextureObject -> IO ()
bindTextureObject uid tx0 = do
  putStrLn $ "Binding Texture Object : " ++ show tx0 ++ " at TextureUnit : " ++ show uid
  texture Texture2D        $= Enabled
  activeTexture            $= TextureUnit uid
  textureBinding Texture2D $= Just tx0

bindTexture :: [(UUID, GLuint)] -> Texture -> IO ()
bindTexture hmap tx =
  do
    putStrLn $ "Binding Texture : " ++ show tx ++ " at TextureUnit : " ++ show txid
    texture Texture2D        $= Enabled
    activeTexture            $= TextureUnit txid
    --activeTexture            $= TextureUnit (DT.trace ("bindTexture.txid : " ++ show txid) txid)
    tx0 <- loadTex $ view path tx --TODO : replace that with a hashmap lookup?
    textureBinding Texture2D $= Just tx0
      where
        txid = fromMaybe 0 (lookup (view uuid tx) hmap)

bindUniforms :: [Texture] -> Uniforms -> Program -> MousePos -> [(UUID, GLuint)] -> IO ()
bindUniforms
  txs
  (Uniforms u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform' u_ypr' u_yprS' u_vel' u_accel')
  u_prog'
  u_mouse'
  hmap =
  do
    let programDebug =
          loadShaders
          [ ShaderInfo VertexShader   (FileSource "./mat/checkerboard/src/shader.vert")   -- u_mat is only used for debug
          , ShaderInfo FragmentShader (FileSource "./mat/checkerboard/src/shader.frag") ]

    program0 <- if debug then programDebug else pure u_prog'

                     
    currentProgram $= Just program0

    let u_mouse0      = Vector2 (realToFrac $ fst u_mouse') (realToFrac $ snd u_mouse') :: Vector2 GLfloat
    location0         <- get (uniformLocation program0 "u_mouse'")
    uniform location0 $= u_mouse0

    let resX          = fromIntegral $ fromEnum $ fst u_res' :: Double
        resY          = fromIntegral $ fromEnum $ snd u_res' :: Double
        u_res         = Vector2 (realToFrac resX) (realToFrac resY) :: Vector2 GLfloat

    location1         <- get (uniformLocation program0 "u_resolution")
    uniform location1 $= u_res

    location2         <- get (uniformLocation program0 "u_time'")
    uniform location2 $= (u_time' :: GLdouble)

    let apt = u_cam_a' -- aperture
        foc = u_cam_f' -- focal length
        proj =
          LP.infinitePerspective
          (2.0 * atan ( apt/2.0 / foc )) -- FOV
          (resX/resY)                    -- Aspect
          0.01                           -- Near

    persp             <- GL.newMatrix RowMajor $ toList' proj   :: IO (GLmatrix GLfloat)
    location3         <- get (uniformLocation program0 "persp")
    uniform location3 $= persp

    --print $ show u_cam'
    camera            <- GL.newMatrix RowMajor $ toList' u_cam' :: IO (GLmatrix GLfloat)
    location4         <- get (uniformLocation program0 "camera")
    uniform location4 $= camera

    xform             <- GL.newMatrix RowMajor $ toList' xform' :: IO (GLmatrix GLfloat)
    location5         <- get (uniformLocation program0 "xform")
    uniform location5 $= xform

    xform1            <- GL.newMatrix RowMajor $ toList' u_xform' :: IO (GLmatrix GLfloat)
    location6         <- get (uniformLocation program0 "xform1")
    uniform location6 $= xform1

    let sunP = GL.Vector3 299999999999.0 0.0 0.0 :: GL.Vector3 GLfloat
    location7 <- get (uniformLocation program0 "sunP")
    uniform location7 $= sunP
    
    let ypr  =
          Vector3
          (double2Float $ u_ypr'^._1)
          (double2Float $ u_ypr'^._2)
          (double2Float $ u_ypr'^._3)
          :: Vector3 GLfloat
    location8        <- get (uniformLocation program0 "ypr")
    uniform location8 $= ypr

    let yprS =
          Vector3
          (double2Float $ u_yprS'^._1)
          (double2Float $ u_yprS'^._2)
          (double2Float $ u_yprS'^._3)
          :: Vector3 GLfloat
    location9        <- get (uniformLocation program0 "yprS")
    uniform location9 $= yprS


    let vel  =
          Vector3
          (double2Float $ u_vel'^._1)
          (double2Float $ u_vel'^._2)
          (double2Float $ u_vel'^._3)
          :: Vector3 GLfloat
    location10        <- get (uniformLocation program0 "vel")
    uniform location10 $= vel

    let accel  =
          Vector3
          (double2Float $ u_accel'^._1)
          (double2Float $ u_accel'^._2)
          (double2Float $ u_accel'^._3)
          :: Vector3 GLfloat
    location11        <- get (uniformLocation program0 "accel")
    uniform location11 $= accel

    --- | Allocate Textures

    -- putStrLn $ "bindUniforms.txNames : "  ++ show txNames
    -- putStrLn $ "bindUniforms.txuids   : " ++ show txuids
    mapM_ (allocateTextures program0 hmap) txs
    --mapM_ (allocateTextures program0 (DT.trace ("bindUniforms.hmap : " ++ show hmap) hmap)) txs

    --- | Unload buffers
    --bindVertexArrayObject         $= Nothing
    --bindBuffer ElementArrayBuffer $= Nothing
      where        
        toList' = fmap realToFrac.concat.(fmap toList.toList) :: V4 (V4 Double) -> [GLfloat]
        xform'  = --- | = Object Position - Camera Position
          transpose $
          fromV3M44
          ( u_xform' ^._xyz )
          ( fromV3V4 (transpose u_xform' ^._w._xyz + transpose u_cam' ^._w._xyz) 1.0 ) :: M44 Double

allocateTextures :: Program -> [(UUID, GLuint)] -> Texture -> IO ()
allocateTextures program0 hmap tx =
  do
    location <- get (uniformLocation program0 (view T.name tx))
    uniform location $= TextureUnit txid
      where
        txid = fromMaybe 0 (lookup (view uuid tx) hmap)

fromList :: [a] -> M44 a
fromList xs = V4
              (V4 (head xs ) (xs!!1 )(xs!!2 )(xs!!3))
              (V4 (xs!!4 ) (xs!!5 )(xs!!6 )(xs!!7))
              (V4 (xs!!8 ) (xs!!9 )(xs!!10)(xs!!11))
              (V4 (xs!!12) (xs!!13)(xs!!14)(xs!!15))

fromV3M44 :: V3 (V4 a) -> V4 a -> M44 a
fromV3M44 v3 = V4 (v3 ^. _x) (v3 ^. _y) (v3 ^. _z)

fromV3V4 :: V3 a -> a -> V4 a
fromV3V4 v3 = V4 (v3 ^. _x) (v3 ^. _y) (v3 ^. _z)

nameFromPath :: FilePath -> String
nameFromPath f = head (splitOn "." $ splitOn "/" f!!1)

toDescriptor :: SVAO' -> IO Descriptor
toDescriptor = initVAO

initVAO :: ([Int], Int, [Float]) -> IO Descriptor
initVAO (idx', st', vs') =
  do
    let
      idx = unsafeCoerce <$> idx' :: [GLuint]
      vs  = unsafeCoerce <$> vs'  :: [GLfloat]
    --- | VAO
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    --- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (length vs * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)
    --- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length idx
    withArray idx $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * sizeOf (head idx))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)

        --- | Bind the pointer to the vertex attribute data
        let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            stride     = fromIntegral st' * floatSize

        --- | Alpha
        vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 1 Float stride ((plusPtr nullPtr . fromIntegral) (0 * floatSize)))
        vertexAttribArray   (AttribLocation 0) $= Enabled
        --- | Colors
        vertexAttribPointer (AttribLocation 1) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (1 * floatSize)))
        vertexAttribArray   (AttribLocation 1) $= Enabled
        --- | Normals
        vertexAttribPointer (AttribLocation 2) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (4 * floatSize)))
        vertexAttribArray   (AttribLocation 2) $= Enabled
        --- | UVW
        vertexAttribPointer (AttribLocation 3) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (7 * floatSize)))
        vertexAttribArray   (AttribLocation 3) $= Enabled
        --- | Positions
        vertexAttribPointer (AttribLocation 4) $= (ToFloat, VertexArrayDescriptor 3 Float stride ((plusPtr nullPtr . fromIntegral) (10 * floatSize)))
        vertexAttribArray   (AttribLocation 4) $= Enabled

    return $ Descriptor vao (fromIntegral numIndices)

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    texture2DWrap            $= (Repeated, ClampToEdge)
    textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
    blend                    $= Enabled
    blendFunc                $= (SrcAlpha, OneMinusSrcAlpha)
    generateMipmap' Texture2D
    return t
