{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiWayIf #-}

module Main where 

import Control.Concurrent
import Control.Monad
import Data.Text                              (Text)
import Data.Maybe
import Foreign.C                              
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
import Foreign.Storable                       (sizeOf)
import FRP.Yampa
import Graphics.Rendering.OpenGL as GL hiding (Size)
import LoadShaders
import Text.Printf

import SDL                             hiding (Point, Event, Timer)
import Input

import Debug.Trace as DT

-- < Rendering > ----------------------------------------------------------
openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
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
     
    window <- SDL.createWindow
              "Mandelbrot Yampa / SDL / OpenGL Example"
              SDL.defaultWindow
              { SDL.windowInitialSize = V2 sizex sizey
              , SDL.windowGraphicsContext = OpenGLContext config
              }      

    SDL.showWindow window
    _ <- SDL.glCreateContext(window)
    
    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window = do
    SDL.destroyWindow window
    SDL.quit

draw :: SDL.Window -> Double -> IO ()
draw window zoom = do
      (Descriptor triangles numIndices) <- initResources verticies indices zoom

      GL.clearColor $= Color4 0 0 0 1
      GL.clear [ColorBuffer]
      bindVertexArrayObject $= Just triangles
      drawElements Triangles numIndices GL.UnsignedInt nullPtr

      SDL.glSwapWindow window

-- < OpenGL > -------------------------------------------------------------
data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices

verticies :: [GLfloat]
verticies =
  [ -- | positions    -- | colors      -- | uv
    1.0,  1.0, 0.0,   1.0, 0.0, 0.0,   1.0, 1.0,
    1.0, -1.0, 0.0,   0.0, 1.0, 0.0,   1.0, 0.0,
   -1.0, -1.0, 0.0,   0.0, 0.0, 1.0,   0.0, 0.0,
   -1.0,  1.0, 0.0,   0.0, 0.0, 0.0,   0.0, 1.0
  ]

indices :: [GLuint]
indices =
  [          -- Note that we start from 0!
    0, 1, 3, -- First Triangle
    1, 2, 3  -- Second Triangle
  ]

initResources :: [GLfloat] -> [GLuint] -> Double -> IO Descriptor
initResources vs idx zoom =  
  do
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length verticies
    withArray verticies $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head verticies))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length indices
    withArray idx $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * (length indices))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 8 * floatSize

    -- | Positions
    let vPosition  = AttribLocation 0
        posOffset  = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | UV
    let uvCoords   = AttribLocation 1
        uvOffset   = 6 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled

    -- || Shaders
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource "Shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/shader.frag")]
    currentProgram $= Just program

    -- || Set Uniforms
    location <- get (uniformLocation program "fTime")
    uniform location $= (realToFrac zoom :: GLfloat)

    -- || Set Transform Matrix
    let tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ] :: [GLfloat]
          
    transform <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location2 <- get (uniformLocation program "transform")
    uniform location2 $= (transform)

    -- || Unload buffers
    bindVertexArrayObject         $= Nothing

    return $ Descriptor triangles (fromIntegral numIndices)
    

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

 -- < Animate > ------------------------------------------------------------

type WinInput = Event SDL.EventPayload
type WinOutput = (Double, Bool)

animate :: Text                   -- ^ window title
        -> CInt                   -- ^ window width in pixels
        -> CInt                   -- ^ window height in pixels
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    window <- openWindow title (winWidth, winHeight)

    lastInteraction <- newMVar =<< SDL.time   
    -- Input Logic -----------------------------------------------------
    let senseInput _ = do
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent
            return (dt, Event . SDL.eventPayload <$> mEvent) 
    -- Output Logic -----------------------------------------------------
        renderOutput _ (zoom, shouldExit) = do
            draw window zoom
            return shouldExit 

    -- Reactimate -----------------------------------------------------
    reactimate (return NoEvent) -- initialize
               senseInput   
               renderOutput
               sf

    closeWindow window

-- < Input Handling > -----------------------------------------------------
updateZoom :: Double -> SF AppInput Double
updateZoom k0 =
  switch sf cont
    where
      sf = proc input -> do
        event1 <- key SDL.ScancodeUp   "Pressed" -< input
        event2 <- key SDL.ScancodeDown "Pressed" -< input
        let res :: (Double, Event (), Event ())
            res = (k0, event1, event2)
        returnA -< (k0, (lMerge event1 event2) `tag` res)
      cont (x,phse, phle) = if | isEvent phse -> zoomIn (x)
--                               | isEvent phle -> contSF2 (x)
                               | otherwise    -> zoomOut (x)

zoomIn :: Double -> SF AppInput Double
zoomIn k0 =
  switch sf cont
    where
         sf = proc input -> do
            zoom    <- DT.trace ("k0: " ++ show k0 ++ "\n") $
                       (k0 +) ^<< integral <<< constant 0.1 -< ()
            event1  <- key SDL.ScancodeUp   "Released"  -< input
            event2  <- key SDL.ScancodeDown "Released"-< input
            returnA -< (zoom, (lMerge event1 event2) `tag` zoom) :: (Double, Event Double)
         cont x = updateZoom (x)

zoomOut :: Double -> SF AppInput Double
zoomOut k0 =
  switch sf cont
    where
         sf = proc input -> do
            zoom    <- DT.trace ("k0: " ++ show k0 ++ "\n") $
                       (k0 -) ^<< integral <<< constant 0.1 -< ()
            event1  <- key SDL.ScancodeUp   "Released" -< input
            event2  <- key SDL.ScancodeDown "Released" -< input
            returnA -< (zoom, (lMerge event1 event2) `tag` zoom) :: (Double, Event Double)            
         cont x = updateZoom (x)

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

-- < Game Types > --------------------------------------------------------------
data Game       = Game { zoom :: Double }
                deriving Show

-- < Game Logic > ---------------------------------------------------------
t0 :: Double
t0 = 0

game :: SF AppInput Game
game = switch sf (\_ -> game)        
     where sf =
             proc input -> do
               gameState <- gameSession  -< input
               reset     <- key SDL.ScancodeQ "Pressed" -< input
               returnA   -< (gameState, reset)

gameSession :: SF AppInput Game
gameSession =
  proc input -> do
     zoom <- updateZoom t0 -< input
     returnA -< Game zoom

-- < Main Function > ------------------------------------------------------

main :: IO ()
main =
     animate "Mandelbrot"
             800
             600
             (parseWinInput >>> ((game >>^ zoom) &&& handleExit))

-- animate "Mandelbrot" 800 600 (parseWinInput >>> ((game >>^ t) &&& handleExit))
--                          game :: SF AppInput Game
--                         (>>^) :: Arrow a => a b c -> (c -> d) -> a b d
--                             t :: Game -> Time
--                  (game >>^ t) :: SF AppInput Time
--                    handleExit :: SF AppInput Bool
--                         (&&&) :: a b c -> a b c' -> a b (c, c')
-- ((game >>^ t) &&& handleExit) :: SF AppInput (Time, Bool)
--                           >>> :: Category cat => cat a b -> cat b c -> cat a c
--                 parseWinInput :: SF WinInput AppInput
-- (parseWinInput >>> ((game >>^ t) &&& handleExit)) :: SF WinInput (Time, Bool)
--                       animate :: Text -> CInt -> CInt -> SF WinInput WinOutput -> IO ()
--                                                                 type WinOutput = (Double, Bool)
--                                                                 type Time      = Double
