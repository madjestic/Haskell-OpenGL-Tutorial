{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}

module Main where 

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Managed
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
import DearImGui as DIG
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
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

      -- SDL.glSwapWindow window

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

-- < Input Handling > -----------------------------------------------------

updateZoom :: Double -> SF AppInput Double
updateZoom z0 =
  switch sf cont
    where
      sf = proc input -> do
        keyQ    <- keyInput (SDL.ScancodeQ) "Pressed" -< input
        keyE    <- keyInput (SDL.ScancodeE) "Pressed" -< input
        let res :: (Double, Event (), Event ())
            res = (z0, keyQ  , keyE  )
        returnA -< (z0, (lMerge keyQ   keyE  ) `tag` res)
      cont (x,phse, phle) = if | isEvent phse -> zoomIn  (x)
                               | otherwise    -> zoomOut (x)

zoomIn :: Double -> SF AppInput Double
zoomIn z0 =
  switch sf cont
    where
         sf = proc input -> do
            zoom    <- DT.trace ("z0: " ++ show z0 ++ "\n") $
                       (z0 +) ^<< integral <<< constant 0.1 -< ()
            keyQ    <- keyInput SDL.ScancodeQ "Released" -< input
            keyE    <- keyInput SDL.ScancodeE "Released" -< input
            returnA -< (zoom, (lMerge keyQ keyE) `tag` zoom) :: (Double, Event Double)
         cont x = updateZoom (x)

zoomOut :: Double -> SF AppInput Double
zoomOut z0 =
  switch sf cont
    where
         sf = proc input -> do
            zoom    <- DT.trace ("z0: " ++ show z0 ++ "\n") $
                       (z0 -) ^<< integral <<< constant 0.1 -< ()
            keyQ    <- keyInput SDL.ScancodeQ "Released" -< input
            keyE    <- keyInput SDL.ScancodeE "Released" -< input
            returnA -< (zoom, (lMerge keyQ keyE) `tag` zoom) :: (Double, Event Double)            
         cont x = updateZoom (x)

instance (Num a,Num b) => Num (a, b) where
  (+)   (a, b) (c, d) = (a+c, b+d)

updatePos :: (Double, Double) -> SF AppInput (Double, Double)
updatePos p0 =
  switch sf cont
    where
      sf = proc input -> do
        keyA    <- keyInput SDL.ScancodeA "Pressed" -< input
        keyD    <- keyInput SDL.ScancodeD "Pressed" -< input
        keyW    <- keyInput SDL.ScancodeW "Pressed" -< input
        keyS    <- keyInput SDL.ScancodeS "Pressed" -< input
        let res :: ((Double, Double)
                   , Event ()
                   , Event ()
                   , Event ()
                   , Event ())
            res =  ( p0
                   , keyA
                   , keyD
                   , keyW
                   , keyS)
        returnA -< (p0, (mergeEvents [ keyA  
                                     , keyD  
                                     , keyW
                                     , keyS ]) `tag` res)

      cont (x, keyA, keyD, keyW, keyS) =
        if | isEvent keyA -> movePos (x) (-0.1, 0.0)
           | isEvent keyD -> movePos (x) ( 0.1, 0.0)
           | isEvent keyW -> movePos (x) ( 0.0, 0.1)  
           | otherwise    -> movePos (x) ( 0.0,-0.1)

movePos :: (Double, Double) -> (Double, Double) -> SF AppInput (Double, Double)
movePos p0 v0 =
  switch sf cont
    where
         sf = proc input -> do
            p       <- DT.trace ("p0: " ++ show p0 ++ "\n") $
                       (p0 +) ^<< integral -< v0
            keyA    <- keyInput SDL.ScancodeA "Released" -< input
            keyD    <- keyInput SDL.ScancodeD "Released" -< input
            keyW    <- keyInput SDL.ScancodeW "Released" -< input
            keyS    <- keyInput SDL.ScancodeS "Released" -< input
            returnA -< (p, (mergeEvents
                            [ keyA  
                            , keyD  
                            , keyW
                            , keyS ]) `tag` p) :: ((Double, Double), Event (Double, Double))
         cont x = updatePos (x)
           
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

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
               reset     <- keyInput (SDL.ScancodeSpace) "Pressed" -< input
               returnA   -< (gameState, reset)

gameSession :: SF AppInput Game
gameSession =
  proc input -> do
     zoom <- updateZoom z0 -< input
     pos  <- updatePos  p0 -< input
     returnA -< Game zoom pos

 -- < Animate > ------------------------------------------------------------
type WinInput  = Event SDL.EventPayload
type WinOutput = ((Double, (Double, Double)), Bool)

animate :: Text                   -- ^ window title
        -> CInt                   -- ^ window width in pixels
        -> CInt                   -- ^ window height in pixels
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    window <- openWindow title (winWidth, winHeight)
    lastInteraction <- newMVar =<< SDL.time
    z <- newMVar (0.0 :: Double)  
    -- Input Logic -----------------------------------------------------
    let senseInput _ = do
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            --mEvent <- SDL.pollEvent                          
            --return (dt, Event . SDL.eventPayload <$> mEvent)
            return (dt, Nothing)
    -- Output Logic -----------------------------------------------------
        renderOutput _ ((zoom, pos), shouldExit) = do
            drawAll window zoom pos z
            return shouldExit 
    -- Reactimate -----------------------------------------------------
    reactimate
      (return NoEvent) -- initialize
      senseInput   
      renderOutput
      sf
    closeWindow window

drawAll :: Window -> Double -> (Double, Double) -> MVar Double -> IO ()
drawAll window z0 p0 z = unlessQuit do
  -- Render
  z' <- readMVar z
  draw window (z0 + z') p0

  -- GUI
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Hello, ImGui!" do
    -- Add a text widget
    text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button "Clickety Click" >>= \case
      False -> return ()
      True  -> do
        z' <- takeMVar z
        putMVar z $ z'+0.1
        putStrLn "Ow!"

  --putMVar z $ z'+0.1

  -- Show the ImGui demo window
  -- showDemoWindow

  render
  openGL3RenderDrawData =<< getDrawData

  SDL.glSwapWindow window

  where
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action

    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent
     

-- < Main Function > ------------------------------------------------------

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

    -- liftIO $ mainLoop window
    liftIO $ animate "Mandelbrot" 800 600 (parseWinInput (800,600) >>> ( ((game >>^ zoom) &&& (game >>^ pos)) &&& handleExit))
