{-# LANGUAGE NamedFieldPuns #-}
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
import Graphics.Rendering.OpenGL as GL hiding (Size, Position)
import NGL.LoadShaders
import System.IO
import System.FilePath
import Control.Exception
import qualified Data.ByteString.Lazy as B

import SDL                             hiding (Point2, Event)

import Input
import Types

-- < Game Types > ---------------------------------------------------------
data Game       = Game { time :: Double  }
                deriving Show

type Clip       = Double

-- < NGL (NGL is not a Graphics Library) > --------------------------------
data Projection = Planar                
                deriving Show 
data Shape      = Square Point2 Size
                deriving Show

type Drawable   = [Vertex4 Double]
type UV         = [TexCoord2 Double]
data Point      = Point2
                | Point3
type Point2     = (Double, Double)
type Point3     = (Double, Double, Double)
type Size       = Double


square :: Point2 -> Double -> [Point2]
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


liftPoint2 :: Point2 -> Point3
liftPoint2 (x,y) = (x, y, 0.0)

liftPoint2s :: [Point2] -> [Point3]
liftPoint2s = map liftPoint2
  

shape2Point2s :: Shape -> [Point2]
shape2Point2s (Square pos side) =  square pos side

toUV :: Projection -> UV
toUV Planar =
  projectPlanar ps
                  where ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
                             ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)] :: [Point2]

toDrawable :: Shape -> Drawable
toDrawable x = map toVertex4 $ shape2Point2s x

toVertex4 :: Point2 -> Vertex4 Double
toVertex4 (k, l)   = Vertex4 k l 0 1

toTexCoord2 :: (a, a) -> TexCoord2 a
toTexCoord2 (k, l) = TexCoord2 k l

projectPlanar :: [Point2] -> UV
projectPlanar      = map $ uncurry TexCoord2                                                                   

-- < Reading PGeo > --------------------------------------------------------
data Tuples = Tuples [Point3] deriving Show

data PGeo =
     PGeo
     {
       tuples :: [Point3] -- tuples of vertices positions
     } deriving Show

instance FromJSON PGeo where
  parseJSON (Object o) =
     PGeo
       <$> ((o .: "PGeo") >>= (.: "tuples"))
  parseJSON _ = mzero

instance FromJSON Tuples where
    parseJSON (Object o) =
      do
        pts <- o .: "tuples"
        fmap Tuples $ parseJSON pts
    parseJSON _ = mzero

type Positions = [Vertex4 Double] 

data Geo =
     Geo
     {
       positions :: [Vertex4 Double]
     } deriving Show

data Transform = Transform {}

defaultGeo :: Geo
defaultGeo = undefined
       
jsonFile :: FilePath
jsonFile = "model.pgeo"           

getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

readPositions :: IO [Point3]
readPositions =
  do
    d <- (eitherDecode <$> getJSON jsonFile) :: IO (Either String PGeo)
    let positions =
          tuples . fromJust $ fromEitherDecode d
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
                  Nothing   -> PGeo []

    return positions

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
    let uv = toUV Planar

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
            draw window ( toDrawable (Square (0.0, 0.0) 1.0)) offset
            return shouldExit 

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
main =
     animate "Mandelbrot" 640 480
                          (parseWinInput >>> ((game >>^ render) &&& handleExit))
