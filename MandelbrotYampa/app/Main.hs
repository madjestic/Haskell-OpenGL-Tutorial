{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where 

import Control.Concurrent
import Control.Monad
import Data.Text                              (Text)
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

-- < Game Types > --------------------------------------------------------------
data Game       = Game { time :: Time }
                deriving Show

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

draw :: SDL.Window -> Double -> IO ()
draw window time = do
      (Descriptor triangles numIndices) <- initResources verticies indices time

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
initResources vs idx time =  
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
        ShaderInfo VertexShader (FileSource "Shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/shader.frag")]
    currentProgram $= Just program

    -- Set Uniforms
    location <- get (uniformLocation program "fTime")
    uniform location $= (realToFrac time :: GLfloat)

    -- Set Transform Matrix
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
    -- bindBuffer ElementArrayBuffer $= Nothing

    -- return $ Descriptor triangles posOffset (fromIntegral numIndices)
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
        renderOutput _ (time, shouldExit) = do
            draw window time
            return shouldExit 

    -- Reactimate -----------------------------------------------------
    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf

    closeWindow window

-- < Input Handling > -----------------------------------------------------

statePassive :: Double -> SF AppInput Double
statePassive k0 =
  switch sf cont
    where
         sf = proc input -> do
            time    <- constant k0 -< ()
            zoomIn   <- pressHoldSpace -< input
            returnA  -< (time, zoomIn `tag` time):: (Double, Event Double)
         cont x = stateTriggered (x)

stateTriggered :: Double -> SF AppInput Double
stateTriggered k0 =
  switch sf cont
    where
         sf = proc input -> do
            time    <- (k0 +) ^<< integral <<< constant 0.1 -< ()
            zoomIn   <- keyReleasedSpace -< input
            returnA  -< (time, zoomIn `tag` time):: (Double, Event Double)
         cont x = statePassive (x)

pressHoldSpace :: SF AppInput (Event ())
pressHoldSpace =
  proc input -> do
    upTapHold   <- keyPressedRepeat (SDL.ScancodeSpace, True) -< input
    upTap       <- keyPressed       (SDL.ScancodeSpace)       -< input
    returnA     -< lMerge upTap upTapHold

keyReleasedSpace :: SF AppInput (Event ())
keyReleasedSpace =
  proc input -> do
    unTap    <- keyReleased      (SDL.ScancodeSpace)       -< input
    returnA  -< unTap

initTime :: Time
initTime = 0

keyPressedQ :: SF AppInput (Event ())
keyPressedQ =
  proc input -> do
    qTap     <- keyPressed ScancodeQ -< input
    returnA  -< qTap

-- < Game Logic > ---------------------------------------------------------
gameSession :: SF AppInput Game
gameSession =
  proc input -> do
     time <- statePassive initTime -< input
     returnA -< Game time

game :: SF AppInput Game
game = switch sf (\_ -> game)        
     where sf =
             proc input -> do
               gameState <- gameSession  -< input
               reset     <- keyPressedQ -< input
               returnA   -< (gameState, reset)

t :: Game -> Time
t (Game time) = time

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

  -- < Main Function > -----------------------------------------------------------
main :: IO ()
main =
     animate "Mandelbrot"
             800
             600
             (parseWinInput >>> ((game >>^ t) &&& handleExit))
