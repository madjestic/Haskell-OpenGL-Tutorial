-- | This variant of Tutorial #3 keeps uses types from the linear
-- package to structure the vertices and colors.  Later tutorials will
-- build on this approach.

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- General Haskell modules
import           Control.Applicative
import           System.FilePath ((</>))

-- Import all OpenGL libraries qualified, for pedagogical reasons
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U
import qualified Linear as L

-- Local modules
-- import qualified Util.GLFW as W
import qualified Graphics.UI.GLFW as W
import qualified Graphics.UI.GLUT as GLUT


main :: IO ()
main = do
    -- GLFW code will be the same in all variants    
    win <- createWindow "My First Window" (512,512)
    prog <- initResources
    mainLoop prog win
    closeWindow win


createWindow :: String -> (Int, Int) -> IO GLFW.Window
createWindow title (sizex,sizey) = do
    GLFW.init
    GLFW.defaultWindowHints
    GLUT.initialDisplayMode $= [GLUT.RGBAMode]
    Just win <- GLFW.createWindow sizex sizey title Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    return win

mainLoop :: Resources -> GLFW.Window -> IO ()
mainLoop prog win = do
         draw prog win
         mainLoop prog win
         

closeWindow :: GLFW.Window -> IO ()
closeWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate


initResources :: IO Resources
initResources = do
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    -- As our shaders take more inputs, collecting the attributes gets
    -- annoying.  GLUtil helps out with the ShaderProgram type, which
    -- keeps track of the 'AttribLocations' and 'UniformLocation's by
    -- name.
    let v = "triangle.v.glsl"
        f = "triangle.f.glsl"
    Resources <$> U.simpleShaderProgram v f
              <*> U.makeBuffer GL.ArrayBuffer vertices
              <*> U.makeBuffer GL.ArrayBuffer colors

draw :: Resources -> GLFW.Window -> IO ()
draw r win = do
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer]
    -- In C++ example GLUT handles resizing the viewport?
    (width, height) <- GLFW.getFramebufferSize win
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    t <- maybe 0 id <$> GLFW.getTime -- time in seconds since program launch
    GL.currentProgram $= (Just . U.program . triProgram $ r)
    -- More helpers from GLUtil, equivalent to:
    -- GL.vertexAttribArray coord2d $= GL.Enabled
    -- GL.vertexAttribArray v_color $= GL.Enabled
    U.enableAttrib (triProgram r) "coord2d"
    U.enableAttrib (triProgram r) "v_color"
    GL.bindBuffer GL.ArrayBuffer $= Just (vertBuffer r)
    U.setAttrib (triProgram r) "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0
    GL.bindBuffer GL.ArrayBuffer $= Just (colorBuffer r)
    U.setAttrib (triProgram r) "v_color"
        GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
    U.setUniform (triProgram r) "fade" (fade t)
    GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices
    -- GLUtil does not yet provide a function to disable attributes
    GL.vertexAttribArray (U.getAttrib (triProgram r) "coord2d") $= GL.Disabled
    GL.vertexAttribArray (U.getAttrib (triProgram r) "v_color") $= GL.Disabled

-- | Represents the shader program and its input buffers
data Resources = Resources { triProgram :: U.ShaderProgram
                           , vertBuffer :: GL.BufferObject
                           , colorBuffer :: GL.BufferObject
                           }

fade :: Double -> GL.Index1 GL.GLfloat
fade t = GL.Index1 . realToFrac $ sin (t * 2 * pi / 5) / 2 + 0.5


shaderPath :: FilePath
shaderPath = "wikibook" </> "tutorial-03-attributes"

vertices :: [L.V2 Float]
vertices = [ L.V2 0.0  0.8
           , L.V2 (-0.8) (-0.8)
           , L.V2 0.8 (-0.8)
           ]

colors :: [L.V3 Float]
colors = [ L.V3 1 1 0
         , L.V3 0 0 1
         , L.V3 1 0 0
         ]
