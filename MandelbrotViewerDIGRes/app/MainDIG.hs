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
import Graphics.RedViz.LoadShaders
import Text.Printf
import Graphics.RedViz.Input.Keyboard
import Graphics.RedViz.Input.FRP.Yampa.AppInput

import SDL                             hiding (Point, Event, Timer)

import Debug.Trace as DT

-- < Rendering > ---------------------------------------------------------------
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

                              -- defaultOpenGL = OpenGLConfig
                              --   { glColorPrecision = V4 8 8 8 0
                              --   , glDepthPrecision = 24
                              --   , glStencilPrecision = 8
                              --   , glMultisampleSamples = 1
                              --   , glProfile = Compatibility Normal 2 1
                              --   }                 

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
closeWindow window = do
    SDL.destroyWindow window
    SDL.quit

draw :: SDL.Window -> Double -> (Double, Double) -> IO ()
draw window z0 p0 = do
      (Descriptor triangles numIndices) <- initResources (verticies p0) indices z0

      GL.clearColor $= Color4 0 0 0 1
      GL.clear [ColorBuffer]
      bindVertexArrayObject $= Just triangles
      drawElements Triangles numIndices GL.UnsignedInt nullPtr

      SDL.glSwapWindow window

-- < OpenGL > -------------------------------------------------------------
data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices

verticies :: (Double, Double) -> [GLfloat]
verticies p0 =
  [ -- | positions    -- | colors      -- | uv
    1.0,  1.0, 0.0,   1.0, 0.0, 0.0,   1.0 + tx, 1.0 + ty,
    1.0, -1.0, 0.0,   0.0, 1.0, 0.0,   1.0 + tx, 0.0 + ty,
   -1.0, -1.0, 0.0,   0.0, 0.0, 1.0,   0.0 + tx, 0.0 + ty,
   -1.0,  1.0, 0.0,   0.0, 0.0, 0.0,   0.0 + tx, 1.0 + ty
  ]
  where
    tx = (\ (x,y)-> realToFrac x) p0 :: GLfloat
    ty = (\ (x,y)-> realToFrac y) p0 :: GLfloat

indices :: [GLuint]
indices =
  [          -- Note that we start from 0!
    0, 1, 3, -- First Triangle
    1, 2, 3  -- Second Triangle
  ]

initResources :: [GLfloat] -> [GLuint] -> Double -> IO Descriptor
initResources vs idx z0 =  
  do
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length vs
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
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
    uniform location $= (realToFrac z0 :: GLfloat)

    -- || Set Transform Matrix
    let tr :: [GLfloat]
        tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ]
          
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
type WinOutput = ((Double, (Double, Double)), Bool)

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
        renderOutput _ ((zoom, pos), shouldExit) = do
            draw window zoom pos
            return shouldExit 

    -- Reactimate -----------------------------------------------------
    reactimate (return NoEvent) -- initialize
               senseInput   
               renderOutput
               sf

    closeWindow window


-- < Game Types > --------------------------------------------------------------
data Game       = Game { zoom :: Double
                       , pos  :: (Double, Double) }
                deriving Show

-- < Game Logic > ---------------------------------------------------------
z0 :: Double
z0 = -3.9053007385999066

p0 :: (Double, Double)
p0 = (-0.15,-0.1)

v0 :: (Double, Double)
v0 = (0,0)

game :: SF AppInput Game
game = switch sf (\_ -> game)        
     where sf =
             proc input -> do
               gameState <- gameSession  -< input
               reset     <- key (SDL.ScancodeSpace) "Pressed" -< input
               returnA   -< (gameState, reset)

gameSession :: SF AppInput Game
gameSession =
  proc input -> do
     zoom <- updateZoom z0 -< input
     pos  <- updatePos  p0 -< input
     returnA -< Game zoom pos

-- < Main Function > ------------------------------------------------------

main :: IO ()
main =
     animate "Mandelbrot"
             800
             600
             (parseWinInput >>> ( ((game >>^ zoom) &&& (game >>^ Main.pos)) &&& handleExit))
