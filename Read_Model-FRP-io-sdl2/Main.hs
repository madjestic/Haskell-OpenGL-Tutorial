{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where 

import Control.Concurrent
import Control.Monad
import Data.Aeson                             hiding (withArray)
import Data.Text                              (Text)
import Foreign.C                              
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
import Foreign.Storable                       (sizeOf)
import FRP.Yampa
import Graphics.Rendering.OpenGL as GL hiding (Size, Position, Point, position)
import NGL.LoadShaders
import System.IO
import System.FilePath
import Control.Exception
import qualified Data.ByteString.Lazy as B

import SDL                             hiding (Point, Vec2, Vec3, Event)

import Input
import Types

-- < Game Types > ---------------------------------------------------------
data Game       = Game { time :: Double  }
                deriving Show

type Clip       = Double

-- < NGL (NGL is not a Graphics Library) > --------------------------------
data Projection = Planar                
                deriving Show 
data Shape2     = Square Vec2 Size deriving Show

data Shape3     = Geo
                  {
                    positions :: [ Vec3 ]
                  }
                deriving Show

type Drawable   = [Vertex4 Double]
type Vec2     = (Double, Double)
type Vec3     = (Double, Double, Double)
type Size       = Double

class Vec2Vertex a where
  toVertex4  :: a -> Vertex4 Double
instance Vec2Vertex Vec2 where
  toVertex4 :: Vec2 -> Vertex4 Double
  toVertex4 (k, l)    = Vertex4 k l 0 1
instance Vec2Vertex Vec3 where
  toVertex4 :: Vec3 -> Vertex4 Double
  toVertex4 (k, l, m) = Vertex4 k l m 1

class Shape2Drawable a where
  toDrawable :: a -> Drawable
instance Shape2Drawable Shape2 where
  toDrawable :: Shape2 -> Drawable
  toDrawable x = map toVertex4 $ toVec2s x
instance Shape2Drawable Shape3 where
  toDrawable :: Shape3 -> Drawable
  toDrawable x = map toVertex4 $ toVec3s x
  
square :: Vec2 -> Double -> [Vec2]
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


liftVec2 :: Vec2 -> Vec3
liftVec2 (x,y) = (x, y, 0.0)

liftVec2s :: [Vec2] -> [Vec3]
liftVec2s = map liftVec2

toVec2s :: Shape2 -> [Vec2]
toVec2s (Square pos side) =  square pos side

toVec3s :: Shape3 -> [Vec3]
toVec3s Geo { positions } = positions

toUV :: Projection -> [TexCoord2 Double]
toUV Planar =
  projectPlanar ps
                  where ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
                             ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)] :: [Vec2]

toTexCoord2 :: (a, a, a) -> TexCoord2 a
toTexCoord2 (k, l, m) = TexCoord2 k l

toTexCoord2s :: [(a, a, a)] -> [TexCoord2 a]
toTexCoord2s = map toTexCoord2 


projectPlanar :: [Vec2] -> [TexCoord2 Double]
projectPlanar      = map $ uncurry TexCoord2
    
-- < Reading PGeo > --------------------------------------------------------
data Position = Position [Vec3] deriving Show
data UV       = UV       [Vec3] deriving Show

data PGeo =
     PGeo
     {
       position :: [Vec3] -- position of vertices positions as Vec3
     , uv       :: [Vec3]
     } deriving Show

instance FromJSON PGeo where
  parseJSON (Object o) =
     PGeo
       <$> ((o .: "PGeo") >>= (.: "position"))
       <*> ((o .: "PGeo") >>= (.: "uv"))
  parseJSON _ = mzero

instance FromJSON Position where
    parseJSON (Object o) =
      do
        pts <- o .: "position"
        fmap Position $ parseJSON pts
    parseJSON _ = mzero

instance FromJSON UV where
    parseJSON (Object o) =
      do
        uv <- o .: "uv"
        fmap UV $ parseJSON uv
    parseJSON _ = mzero

type Positions = [Vertex4 Double] 

data Transform = Transform {}


jsonFile :: FilePath
jsonFile = "model.pgeo"           

getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

readPGeo :: IO ([Vec3], [Vec3])
readPGeo =
  do
    d <- (eitherDecode <$> getJSON jsonFile) :: IO (Either String PGeo)
    let positions =
          position . fromJust $ fromEitherDecode d
          where
            fromEitherDecode d =
              do
                case d of
                  Left err -> Nothing
                  Right ps -> Just ps
                  
            fromJust pgeo =
              do
                case pgeo of
                  Just pgeo -> pgeo
                  Nothing   -> PGeo [] []

    let uvs =
          uv . fromJust $ fromEitherDecode d
          where
            fromEitherDecode d =
              do
                case d of
                  Left err -> Nothing
                  Right ps -> Just ps
                  
            fromJust pgeo =
              do
                case pgeo of
                  Just pgeo -> pgeo
                  Nothing   -> PGeo [] []

    return (positions, uvs)

-- < Rendering > ----------------------------------------------------------
openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"  
     
    window <- SDL.createWindow
            "Mandelbrot Yampa / SDL / OpenGL Example"
            SDL.defaultWindow {SDL.windowInitialSize = V2 sizex sizey,
                               SDL.windowOpenGL = Just SDL.defaultOpenGL}
    SDL.showWindow window
    _ <- SDL.glCreateContext(window)
    
    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window = do
    SDL.destroyWindow window
    SDL.quit

draw :: SDL.Window -> Drawable -> Double -> IO ()
draw window drawable offset = do
      (Descriptor triangles firstIndex numVertices) <- initResources drawable offset

      GL.clearColor $= Color4 0 0 0 1
      GL.clear [ColorBuffer]
      bindVertexArrayObject $= Just triangles
      drawArrays Triangles firstIndex numVertices

      SDL.glSwapWindow window

-- < OpenGL > -------------------------------------------------------------
data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

initResources :: ([Vertex4 Double]) -> Double -> IO Descriptor
initResources (vs) offset = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    --
    -- Declaring VBO: vertices
    --
    let vertices = vs
        numVertices = length vertices

    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    withArray vs $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 4 GL.Double 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    --
    -- Declaring VBO: UVs
    --
    -- let uv = toUV Planar
    (_, uvs) <- readPGeo
    let uv = toTexCoord2s uvs

    textureBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just textureBuffer
    withArray uv $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head uv))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let uvCoords = AttribLocation 1
    vertexAttribPointer uvCoords $=
        (ToFloat, VertexArrayDescriptor 2 GL.Double 0 (bufferOffset firstIndex))
    vertexAttribArray uvCoords   $= Enabled

    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "Shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/shader.frag")]
    currentProgram $= Just program

    -- Set Uniforms
    location <- get (uniformLocation program "fTime")
    uniform location $= (realToFrac offset :: GLfloat)

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)    

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

-- < Animate > ------------------------------------------------------------

type WinOutput = (Double, Bool)

animate :: Text                   -- ^ window title
        -> CInt                   -- ^ window width in pixels
        -> CInt                   -- ^ window height in pixels
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    window <- openWindow title (winWidth, winHeight)

    lastInteraction <- newMVar =<< SDL.time   
      
    let senseInput _ = do
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent                          
            return (dt, Event . SDL.eventPayload <$> mEvent) 

        renderOutput _ (offset, shouldExit) = do
            (ps, _) <- readPGeo
            let geo = Geo ps
            draw window ( toDrawable geo) offset
            return shouldExit 
            -- draw window ( toDrawable (Square (0.0, 0.0) 1.0)) offset

    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf

    closeWindow window

-- < Input Handling > -----------------------------------------------------

stateReleased :: Double -> SF AppInput Double
stateReleased k0 =
  switch sf cont
    where
         sf = proc input -> do
            offset    <- constant k0 -< ()
            zoomIn   <- trigger -< input
            returnA  -< (offset, zoomIn `tag` offset):: (Double, Event Double)
         cont x = stateTriggered (x)

stateTriggered :: Double -> SF AppInput Double
stateTriggered k0 =
  switch sf cont
    where
         sf = proc input -> do
            offset    <- (k0 +) ^<< integral <<< constant 0.1 -< ()
            zoomIn   <- release -< input
            returnA  -< (offset, zoomIn `tag` offset):: (Double, Event Double)
         cont x = stateReleased (x)

trigger :: SF AppInput (Event ())
trigger =
  proc input -> do
    upTapHold   <- keyPressedRepeat (SDL.ScancodeSpace, True) -< input
    upTap       <- keyPressed       (SDL.ScancodeSpace)       -< input
    returnA     -< lMerge upTap upTapHold

release :: SF AppInput (Event ())
release =
  proc input -> do
    unTap    <- keyReleased      (SDL.ScancodeSpace)       -< input
    returnA  -< unTap

initClip :: Double
initClip = 0

exitTrigger :: SF AppInput (Event ())
exitTrigger =
  proc input -> do
    qTap     <- keyPressed ScancodeQ -< input
    returnA  -< qTap

-- < Game Logic > ---------------------------------------------------------
gameSession :: SF AppInput Game
gameSession = proc input -> do
     offset <- stateReleased initClip -< input
     returnA -< Game offset

game :: SF AppInput Game
game = switch sf (\_ -> game)        
     where sf = proc input -> do
                     gameState <- gameSession  -< input
                     gameOver  <- exitTrigger  -< input
                     returnA   -< (gameState, gameOver)

render :: Game -> Time
render (Game time) = time

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main = do
     animate "Mandelbrot" 640 480
                         (parseWinInput >>> ((game >>^ render) &&& handleExit))

