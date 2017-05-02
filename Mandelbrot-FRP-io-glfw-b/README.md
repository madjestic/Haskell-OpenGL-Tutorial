DISCLAMER: BROKEN!
It's an attempt to create an animated Mandelbrot example, using [FRP.Yampa](https://github.com/ivanperez-keera/Yampa) to handle animation loop,  GLFW-b is used for windows and event callbacks, OpenGL is used for rendering.
Shader-loading is handled by [Sven Panne's code](https://github.com/haskell-opengl/GLUT/blob/master/examples/RedBook8/common/LoadShaders.hs).

![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/Mandelbrot-FRP-io-sdl2/output.png)

in order to run: 

```
  $ make
  $ optirun -b primus ./Main
```

Controls:
```
  space - zoom in
  q     - reset
```