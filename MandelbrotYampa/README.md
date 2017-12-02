# MandelbrotYampa

A simple [OpenGL](https://github.com/haskell-opengl) application, using [FRP.Yampa](https://github.com/ivanperez-keera/Yampa) to handle animation and user events,  SDL2 for managing windows and input, [stack](https://docs.haskellstack.org/en/stable/README/) as a build system.
Shader-loading is handled by [Sven Panne's code](https://github.com/haskell-opengl/GLUT/blob/master/examples/RedBook8/common/LoadShaders.hs).
Input handling is inspired and based on [Konstantin Zudov, Yampy Cube](https://github.com/zudov) presentation at Helsinki User Group.

## Prerequisites:
   Hardware, supporting OpenGL >= 4.5 (because that's what my hardware is).
   Most likely the code will run with lesser OpenGL versions, but you will
   have to mess with the code.

## In order to run:
```
  stack build
  stack exec Mandelbrot
  # or, in case you are running a hybrid graphics like myself, use a launch script, e.g.:
  gpu ./run.sh
  
```

## Controls:
```
  space - zoom in
  q     - reset
```

## Output:
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/MandelbrotYampa/output.png)

## Animated Output:
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/MandelbrotYampa/output.gif)
