An animated Mandelbrot example, using [FRP.Yampa](https://github.com/ivanperez-keera/Yampa) to handle animation loop and events,  SDL2 is used for windows and input, OpenGL (NGL is Not a Graphics Library) is used for rendering.
Shader-loading is handled by [Sven Panne's code](https://github.com/haskell-opengl/GLUT/blob/master/examples/RedBook8/common/LoadShaders.hs).
Input handling is inspired and based on [Konstantin Zudov, Yampy Cube](https://github.com/zudov) presentation at Helsinki User Group.

![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/Mandelbrot-FRP-io-sdl2/output.png)

in order to run: 

```bash
  $ make
  $ optirun -b primus ./Main
```

Controls:
```
  space - zoom in
  q     - reset
```