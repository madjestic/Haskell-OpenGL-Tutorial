import Graphics.Rendering.OpenGL as GL

vertices = [
  Vertex2 (-0.90) (-0.90),  -- Triangle 1
  Vertex2   0.85  (-0.90),
  Vertex2 (-0.90)   0.85 ,
  Vertex2   0.90  (-0.85),  -- Triangle 2
  Vertex2   0.90    0.90 ,
  Vertex2 (-0.85)   0.90 ] :: [Vertex2 GLfloat]


rawVerts = [
  [(-0.90), (-0.90)],  -- Triangle 1
  [  0.85 , (-0.90)],
  [(-0.90),   0.85 ],
  [  0.90 , (-0.85)],  -- Triangle 2
  [  0.90 ,   0.90 ],
  [(-0.85),   0.90 ]]

--toGLvertices :: [[Double]] -> [Vertex2 GLfloat]
