module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import LoadShaders
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import HNGL


bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices


initResources :: IO Descriptor
initResources = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    let vertices = toDrawable (Square v1 1.0)
        numVertices = length vertices

    arrayBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just arrayBuffer
    withArray vertices $ \ptr -> do
        let size = fromIntegral (numVertices * sizeOf (head vertices))
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "triangles.vert"),
        ShaderInfo FragmentShader (FileSource "triangles.frac")]
    currentProgram $= Just program

    let firstIndex = 0
        vPosition = AttribLocation 0
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset firstIndex))
    vertexAttribArray vPosition $= Enabled

    return $ Descriptor triangles firstIndex (fromIntegral numVertices)

                    
resizeWindow :: Size -> IO ()
resizeWindow size@(GL.Size w h) =
    do
        GL.viewport   $= (GL.Position 0 0, size)
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (realToFrac w) (realToFrac h) 0


main :: IO ()
main = do
    GLFW.initialize
    GLFW.openWindow (GL.Size 640 480) [] GLFW.Window
    GLFW.windowTitle $= "GLFW Demo"
    GLFW.windowSizeCallback $= resizeWindow
    descriptor <- initResources
    onDisplay descriptor
    GLFW.closeWindow
    GLFW.terminate


onDisplay :: Descriptor -> IO ()
onDisplay descriptor@(Descriptor triangles firstIndex numVertices) = do
    GL.clearColor $= Color4 1 0 0 1
    GL.clear [ColorBuffer]
    bindVertexArrayObject $= Just triangles
    drawArrays Triangles firstIndex numVertices
    GLFW.swapBuffers

    p <- GLFW.getKey GLFW.ESC
    unless (p == GLFW.Press) $ onDisplay descriptor

