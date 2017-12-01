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
import Types

-- < Game Types > --------------------------------------------------------------
data Game       = Game { timer :: Timer  }
                deriving Show

type Timer       = Double

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
draw window timer = do
      -- (Descriptor triangles firstIndex numVertices) <- initResources drawable timer
      (Descriptor triangles numIndices) <- initResources verticies indices timer

      GL.clearColor $= Color4 0 0 0 1
      GL.clear [ColorBuffer]
      bindVertexArrayObject $= Just triangles
      drawElements Triangles numIndices GL.UnsignedInt nullPtr

      SDL.glSwapWindow window

-- < OpenGL > -------------------------------------------------------------
data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices

data GLMatrix a =
     GLMatrix !a !a !a !a
              !a !a !a !a
              !a !a !a !a
              !a !a !a !a
                deriving Eq

instance PrintfArg a => Show (GLMatrix a) where
  show (GLMatrix m11 m12 m13 m14
                 m21 m22 m23 m24
                 m31 m32 m33 m34
                 m41 m42 m43 m44) =
    let matrix = "[ %v %v %v %v ]\n\
                 \[ %v %v %v %v ]\n\
                 \[ %v %v %v %v ]\n\
                 \[ %v %v %v %v ]\n"
    in printf matrix m11 m12 m13 m14
                     m21 m22 m23 m24
                     m31 m32 m33 m34
                     m41 m42 m43 m44

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

-- initResources :: ([Vertex4 Double]) -> Double -> IO Descriptor
-- initResources (vs) timer =
initResources :: [GLfloat] -> [GLuint] -> Double -> IO Descriptor
initResources vs idx timer =  
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
    uniform location $= (realToFrac timer :: GLfloat)

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
        renderOutput _ (timer, shouldExit) = do
            draw window timer
            return shouldExit 

    -- Reactimate -----------------------------------------------------
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
            timer    <- constant k0 -< ()
            zoomIn   <- trigger -< input
            returnA  -< (timer, zoomIn `tag` timer):: (Double, Event Double)
         cont x = stateTriggered (x)

stateTriggered :: Double -> SF AppInput Double
stateTriggered k0 =
  switch sf cont
    where
         sf = proc input -> do
            timer    <- (k0 +) ^<< integral <<< constant 0.1 -< ()
            zoomIn   <- release -< input
            returnA  -< (timer, zoomIn `tag` timer):: (Double, Event Double)
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

initTimer :: Timer
initTimer = 0

exitTrigger :: SF AppInput (Event ())
exitTrigger =
  proc input -> do
    qTap     <- keyPressed ScancodeQ -< input
    returnA  -< qTap

-- < Game Logic > ---------------------------------------------------------
gameSession :: SF AppInput Game
gameSession = proc input -> do
     timer <- stateReleased initTimer -< input
     returnA -< Game timer

game :: SF AppInput Game
game = switch sf (\_ -> game)        
     where sf = proc input -> do
                     gameState <- gameSession  -< input
                     gameOver  <- exitTrigger -< input
                     returnA   -< (gameState, gameOver)

render :: Game -> Timer
render (Game timer) = timer

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

-- < Main Function > -----------------------------------------------------------
main :: IO ()
main =
     animate "Mandelbrot" 800 600
                          (parseWinInput >>> ((game >>^ render) &&& handleExit))
