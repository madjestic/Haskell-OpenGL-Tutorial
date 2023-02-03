# MandelbrotViewer

A further development of [MandelbrotYampa](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/MandelbrotYampa) with extended IO.

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
  ( stack build && gpu ./run.sh )
  
```

## Controls:
```
  Q       - zoom in
  R       - zoom out
  A,D,W,S - Left, Right, Up, Down
  Space   - reset
  Escape  - quit
```

## Output:
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/MandelbrotViewerDIG/output.png)

## Animated Output:
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/MandelbrotViewerDIG/output.png)

