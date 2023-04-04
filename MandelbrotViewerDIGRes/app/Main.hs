{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE FlexibleInstances #-}

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
import Graphics.RedViz.Rendering hiding (render)
import Graphics.RedViz.Descriptor ( Descriptor(..) )
import GHC.Generics  

import SDL                             hiding (Point, Event, Timer)

import Debug.Trace as DT

-- < Rendering > ---------------------------------------------------------------
draw :: SDL.Window -> Double -> (Double, Double) -> IO ()
draw window z0 p0 = do
      (Descriptor triangles numIndices) <- initResources (verticies p0) indices z0

      GL.clearColor $= Color4 0 0 0 1
      GL.clear [ColorBuffer]
      bindVertexArrayObject $= Just triangles
      drawElements Triangles numIndices GL.UnsignedInt nullPtr

      -- SDL.glSwapWindow window

-- < OpenGL > -------------------------------------------------------------

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

-- TODO: try moving init Descriptor outside of the draw loop
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

--bar :: SF AppInput (MVar Double)
-- bar :: _  
-- bar = do
--   proc _ -> do
--     foo = newMVar (0.0 :: Double)
--     returnA -< foo

  -- < Game Types > --------------------------------------------------------------
data Game       = Game { zoom     :: Double
                       , pos      :: (Double, Double)
                       , zoomMVar :: MVar Double
                       } -- deriving (Generic, Show)

-- instance Show (MVar Double) where
--   show _ = ""

  -- < Game Logic > ---------------------------------------------------------
z0 :: Double
z0 =  0.0

p0 :: (Double, Double)
p0 = (-0.15,-0.1)

v0 :: (Double, Double)
v0 = (0,0)

game :: SF AppInput Game
game = switch sf (\_ -> game)        
  where
    sf =
      proc input -> do
        game <- gameSession  -< input
        reset     <- keyInput (SDL.ScancodeSpace) "Pressed" -< input
        returnA   -< (game, reset)

gameSession :: SF AppInput Game
gameSession =
  proc input -> do
    zoom <- updateZoom z0 -< input
    pos  <- updatePos  p0 -< input
    returnA -< undefined --Game zoom pos

 -- < Animate > ------------------------------------------------------------
-- type WinInput  = Event SDL.EventPayload
-- type WinOutput = (Game, Bool)
-- type WinOutput = ((Double, (Double, Double)), Bool)

-- animate :: GLContext
--         -> SDL.Window
--         -> SF WinInput WinOutput  -- ^ signal function to animate
--         -> IO ()
-- animate glContext window sf = do
--   --z <- newMVar (0.0 :: Double)
--   --let z = (0.0 :: Double)
--   runManaged do
--     z <- liftIO $ newMVar (0.0 :: Double)
--     -- Create an ImGui context
--     _ <- managed $ bracket createContext destroyContext
--     -- Initialize ImGui's SDL2 backend
--     _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
--     -- Initialize ImGui's OpenGL backend
--     _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown
--     -- Input Logic -----------------------------------------------------
--     let
--       input _ = do
--         lastInteraction <- newMVar =<< SDL.time
--         currentTime <- SDL.time                          
--         dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
--         --mEvent <- SDL.pollEvent                          
--         --return (dt, Event . SDL.eventPayload <$> mEvent)
--         return (dt, Nothing)
--     -- Output Logic -----------------------------------------------------
--       output _ ((zoom, pos), shouldExit) = do
--         -- z <- newMVar (0.0 :: Double)
--         drawAll window zoom pos z
--         return shouldExit 
--     -- Reactimate -----------------------------------------------------
--     liftIO $
--       reactimate
--       (return NoEvent) -- initialize
--       input   
--       output
--       sf
--     liftIO $ closeWindow window

type WinInput  = (Event SDL.EventPayload)
type WinOutput = (Game, Bool)

animate :: GLContext
        -> SDL.Window
        -> MVar Bool
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate glContext window switch0 sf = do
  -- let (game', _) = head $ embed sf (NoEvent, [(0.0, Nothing)])
  -- print $ zoom game'

  runManaged do
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext
    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown
    --inputSwitch' <- liftIO $ tryReadMVar switch0
    
    -- Reactimate -----------------------------------------------------
    liftIO $
      reactimate
      (return NoEvent) -- initialize
      input
      output
      sf
    liftIO $ closeWindow window
    -- Input Logic -----------------------------------------------------
    where
      input _ = do
        switch0' <- readMVar switch0
        case switch0' of
          False -> do
            lastInteraction <- newMVar =<< SDL.time
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            return (dt, Nothing)
          True -> do
            lastInteraction <- newMVar =<< SDL.time
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent                          
            return (dt, Event . SDL.eventPayload <$> mEvent)

    -- Output Logic -----------------------------------------------------
      output _ (game, shouldExit) = do
        readMVar switch0 >>= print
        drawAll window game
        z' <- readMVar $ zoomMVar game
        print $ "z' :" ++ show z'
        if z' > 0.3
          then do
          _ <- swapMVar switch0 True
          return shouldExit
          else do
          return shouldExit
  
drawAll :: Window -> Game -> IO ()
drawAll window game = unlessQuit do
  let
    z0 = zoom game
    p0 = pos  game
  -- Render
  z' <- readMVar $ zoomMVar game
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
        z' <- takeMVar $ zoomMVar game
        putMVar (zoomMVar game) $ z' + 0.1
        putStrLn "Ow!"
        

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

mainLoop :: Game -> SF AppInput Game
mainLoop game0 = 
  loopPre game0 $
  proc (input, game) -> do
    game1   <- returnA -< game0
    returnA -< (game1, game1)
  --   app1 <- appMain  app0 -< (input, game)
  --   returnA -< (app1, app1)

-- appMain :: Game -> SF (AppInput, Game) Game
-- appMain game0 =
--   proc (input, app1) -> do
--     returnA -< app1
    -- app'        <- updateMainApp (fromApplication app0) -< (input, app1 ^. main)
    -- reset       <- keyInput SDL.ScancodeSpace "Pressed" -< input
    -- zE          <- keyInput SDL.ScancodeZ     "Pressed" -< input

    -- let
    --   result =
    --     app1 { _main = app' }
               
    -- returnA     -< if isEvent reset 
    --                then (result, reset $> app0   { Appl._gui = app1 ^. intr . App.gui } )
    --                else (result, zE    $> result { Appl._gui = fromSelected app1 app' } )

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
    z <- liftIO $ newMVar (0.0 :: Double)
    switch' <- liftIO $ newMVar False
    --_ <- liftIO $ swapMVar switch' True
    --foo <- liftIO $ readMVar switch'
    --liftIO $ print foo

         --liftIO $ animate glContext window (parseWinInput (800,600) >>> ( ((game >>^ zoom) &&& (game >>^ pos) ) &&& handleExit))
    let
      initGame =
        Game
        {
          zoom = 0.0 :: Double
        , pos  = (0.0, 0.0) :: (Double, Double)
        , zoomMVar = z
        }
      
    --liftIO $ animate glContext window (parseWinInput (800,600) >>> ( ((game >>^ zoom) &&& (game >>^ pos) ) &&& handleExit))
    liftIO $ animate glContext window switch' (parseWinInput (800,600) >>> mainLoop initGame &&& handleExit )
