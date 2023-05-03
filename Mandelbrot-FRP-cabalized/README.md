An animated Mandelbrot example, using [FRP.Yampa](https://github.com/ivanperez-keera/Yampa) to handle animation loop,  GLFW-b is used for windows and event callbacks.
Shader-loading is handled by [Sven Panne's code](https://github.com/haskell-opengl/GLUT/blob/master/examples/RedBook8/common/LoadShaders.hs).

![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/Mandelbrot/output.png)

in order to run: 

```bash
  $ make
  $ optirun -b primus ./Main
```