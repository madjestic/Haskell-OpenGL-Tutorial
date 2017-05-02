{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where 

import Control.Concurrent
import Control.Monad
import Data.Text                              (unpack, Text)
import Foreign.C                              
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
import Foreign.Storable                       (sizeOf)
import FRP.Yampa
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import SDL hiding (Point, Event)
import System.Exit                         (exitSuccess)

import NGL.LoadShaders
import Input hiding (keyPressed)
import Types

-- < Game Types > --------------------------------------------------------------
data Game       = Game { clipPos :: Double  }
                deriving Show

type Clip       = Double

-- < NGL (NGL is not a Graphics Library) > --------------------------------
data Projection = Planar                
                deriving Show 
data Shape      = Square Point   Side
                deriving Show

type Drawable   = [Vertex4 Double]
type UV         = [TexCoord2 Double] 
type Point      = (Double, Double)
type Points     = [Point]     
type Side       = Double


square :: Point -> Double -> [Point]
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

toPoints :: Shape -> [Point]
toPoints (Square pos side) =  square pos side

toUV :: Projection -> UV
toUV Planar =
  projectPlanar ps
                  where ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
                             ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)]::Points

toDrawable :: Shape -> Drawable
toDrawable x = map toVertex4 $ toPoints x

toVertex4 :: Point -> Vertex4 Double
toVertex4 (k, l)   = Vertex4 k l 0 1

toTexCoord2 :: (a, a) -> TexCoord2 a
toTexCoord2 (k, l) = TexCoord2 k l

projectPlanar :: [Point] -> UV
projectPlanar      = map $ uncurry TexCoord2                                                                   

-- < Rendering > ----------------------------------------------------------
openWindow :: Text -> (Int, Int) -> IO GLFW.Window
openWindow title (sizex,sizey) = do
    GLFW.init
    GLFW.defaultWindowHints
    Just window <- GLFW.createWindow sizex sizey (unpack title) Nothing Nothing
    GLFW.makeContextCurrent (Just window)
    return window

closeWindow :: GLFW.Window -> IO ()
closeWindow window = do
    GLFW.destroyWindow window
    GLFW.terminate

draw :: GLFW.Window -> Drawable -> Double -> IO ()
draw window drawable timer = do
      (Descriptor triangles firstIndex numVertices) <- initResources drawable timer

      GL.clearColor $= Color4 0 0 0 1
      GL.clear [ColorBuffer]
      bindVertexArrayObject $= Just triangles
      drawArrays Triangles firstIndex numVertices

      GLFW.swapBuffers window

-- < OpenGL > -------------------------------------------------------------
data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

initResources :: ([Vertex4 Double]) -> Double -> IO Descriptor
initResources (vs) timer = do
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
    uniform location $= (realToFrac timer :: GLfloat)

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)    

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

-- < GLFW-b Callbacks > ----------------------------------------------------------
keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()


shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitSuccess
  return ()                                                                  


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
    do
      GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0   

-- < Animate > ------------------------------------------------------------
type WinOutput = (Double, Bool)

animate :: Text                   -- ^ window title
        -> Int                    -- ^ window width in pixels
        -> Int                    -- ^ window height in pixels
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    window <- openWindow title (winWidth, winHeight)

    lastInteraction <- newMVar =<< GLFW.getTime
      
    let senseInput _canBlock = do
            currentTime <- GLFW.getTime
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            GLFW.setWindowSizeCallback window (Just resizeWindow)
            GLFW.setKeyCallback window (Just keyPressed)
            GLFW.setWindowCloseCallback window (Just shutdown)
            mEvent <- GLFW.pollEvents
            return (dt, Event . SDL.eventPayload' <$> mEvent) -- don't know how map this to glfw-b

        renderOutput changed (timer, shouldExit) = do
            when changed $ do
                 draw window ( toDrawable (Square (0.0, 0.0) 1.0)) timer
            return shouldExit 

    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf

    closeWindow window

-- < Input Handling > -----------------------------------------------------
counter :: Double -> SF AppInput Double
counter k0 =
  switch sf cont
    where
         sf = proc input -> do
            timer    <- constant k0 -< ()
            zoomIn   <- trigger -< input
            returnA  -< (timer, zoomIn `tag` timer):: (Double, Event Double)
         cont x = counter (x + 0.1)

trigger :: SF AppInput (Event ())
trigger =
  proc input -> do
    upTap   <- keyPressed ScancodeSpace -< input
    returnA -< upTap

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
     timer <- counter initClip -< input
     returnA -< Game timer

game :: SF AppInput Game
game = switch sf (\_ -> game)        
     where sf = proc input -> do
                     gameState <- gameSession  -< input
                     gameOver  <- exitTrigger -< input
                     returnA   -< (gameState, gameOver)

render :: Game -> Clip
render (Game clipPos) = clipPos

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main =
     animate "Mandelbrot" 640 480
                          (parseWinInput >>> ((game >>^ render) &&& handleExit))
