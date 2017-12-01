# Mandelbrot

A simple [OpenGL](https://github.com/haskell-opengl) application, drawing a Mandebrot fractal, using [stack](https://docs.haskellstack.org/en/stable/README/) as a build system.

## Prerequisits:
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

## Output:
![](https://raw.githubusercontent.com/madjestic/Haskell-OpenGL-Tutorial/master/Mandelbrot/output.png)
