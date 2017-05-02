{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
module Main where 

import Control.Concurrent
import Data.Text                           (unpack, Text)
import Foreign.Marshal.Array               (withArray)
import Foreign.Ptr                         (plusPtr, nullPtr, Ptr)
import Foreign.Storable                    (sizeOf)
import FRP.Yampa
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.GLUtil                     (readTexture, texture2DWrap)
import NGL.LoadShaders
import System.Exit                         (exitSuccess)


data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices
data Projection = Planar                
                deriving Show 
data Shape = Square    Point   Side
                deriving Show     

type Pos        = Double
type Vel        = Double     

type Drawable   = ([Vertex4 Double],[TexCoord2 Double],String)
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
toUV Planar = projectPlanar ps
                  where ps = [(1.0, 1.0),( 0.0, 1.0),( 0.0, 0.0)
                             ,(1.0, 1.0),( 0.0, 0.0),( 1.0, 0.0)]::Points


toDrawable :: Shape -> Drawable
toDrawable x = (vs, uv, tex)
                  where
                      vs'  = toPoints x               
                      uv   = map toTexCoord2 vs'
                      vs   = map toVertex4 vs'
                      tex  = "test.png"                                                                   


toVertex4 :: Point -> Vertex4 Double
toVertex4 (k, l)   = Vertex4 k l 0 1


toTexCoord2 :: (a, a) -> TexCoord2 a
toTexCoord2 (k, l) = TexCoord2 k l


projectPlanar :: [Point] -> UV
projectPlanar      = map $ uncurry TexCoord2                                                                   


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


openWindow :: Text -> (Int, Int) -> IO GLFW.Window
openWindow title (sizex,sizey) = do
    GLFW.init
    GLFW.defaultWindowHints
    Just win <- GLFW.createWindow sizex sizey (unpack title) Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    GLFW.setWindowSizeCallback win (Just resizeWindow)
    GLFW.setKeyCallback win (Just keyPressed)
    GLFW.setWindowCloseCallback win (Just shutdown)
    return win


closeWindow :: GLFW.Window -> IO ()
closeWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate    


draw :: GLFW.Window -> Drawable -> Double -> IO ()
draw win drawable timer = do
           (Descriptor triangles firstIndex numVertices) <- initResources drawable timer

           GL.clearColor $= Color4 0 0 0 1
           GL.clear [ColorBuffer]
           bindVertexArrayObject $= Just triangles
           drawArrays Triangles firstIndex numVertices

           GLFW.swapBuffers win
           GLFW.pollEvents    


initResources :: ([Vertex4 Double],[TexCoord2 Double],String) -> Double -> IO Descriptor
initResources (vs, uv, tex) timer = do
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
        (ToFloat, VertexArrayDescriptor 4 Double 0 (bufferOffset firstIndex))
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
        (ToFloat, VertexArrayDescriptor 2 Double 0 (bufferOffset firstIndex))
    vertexAttribArray uvCoords $= Enabled

    tx <- loadTex tex
    texture Texture2D $= Enabled
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just tx    

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


loadTex :: FilePath -> IO TextureObject
loadTex f = do t <- either error id <$> readTexture f
               textureFilter Texture2D $= ((Linear', Nothing), Linear')
               texture2DWrap $= (Repeated, ClampToEdge)
               return t                   


animate :: Text                  -- ^ window title
        -> Int                   -- ^ window width in pixels
        -> Int                   -- ^ window height in pixels
        -> SF () (Double)        -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    win <- openWindow title (winWidth, winHeight)

    -- | main loop
    reactimate (return ()) 
               (\_ -> threadDelay 5000 >> return (0.1, Nothing))
               (\_ timer ->  draw win ( toDrawable (Square (0.0, 0.0) 1.0)) timer >> return False)
               sf

    closeWindow win        


counter :: Double -> SF () Double
counter k = proc _ -> do
                      timer <- (k +) ^<< integral <<< constant 1 -< ()
                      returnA -< timer


main :: IO ()
main =
     animate "Mandelbrot" (round 640) (round 480) (counter 0)           
