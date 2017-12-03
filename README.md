[![Build Status](https://travis-ci.org/madjestic/Haskell-OpenGL-Tutorial.svg?branch=master)](https://travis-ci.org/madjestic/Haskell-OpenGL-Tutorial)

# Haskell-OpenGL-Tutorial
an attempt to create a concise modern Haskell OpenGL boilerplate with basic IO among other things...
## [MandelbrotYampa](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/MandelbrotYampa)

A simple [OpenGL](https://github.com/haskell-opengl) application, using [FRP.Yampa](https://github.com/ivanperez-keera/Yampa) to handle animation and user events,  SDL2 for managing windows and input, [stack](https://docs.haskellstack.org/en/stable/README/) as a build system.
```
windows and input: SDL2  
shader uniforms  : +  
frp              : Yampa
```
### Output:
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/MandelbrotYampa/output.png)
### Animated Output:
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/MandelbrotYampa/output.gif)
\
\
\
## [A GLFW Boilerplate](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/Boilerplate)
An OpenGL template:
```
windows and input: GLFW  
shader uniforms  : -
```
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/Boilerplate/output.png)
\
\
\
## [A GLFW Boilerplate with Mandelbrot shader](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/Mandelbrot)
```
windows and input: GLFW  
shader uniforms  : +
```
![](https://raw.githubusercontent.com/madjestic/Haskell-OpenGL-Tutorial/master/Mandelbrot/output.png)


## [A SDL2 Boilerplate with Mandelbrot shader](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/Mandelbrot-FRP-io-sdl2)
```
windows and input: SDL2  
shader uniforms  : +
```
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/Mandelbrot-FRP-io-sdl2/output.png)


## [HelloWindow](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/HelloWindow)
A Hello Window [OpenGL](https://github.com/haskell-opengl) application with [stack](https://docs.haskellstack.org/en/stable/README/) as a build system.
### Output:
![](https://raw.githubusercontent.com/madjestic/Haskell-OpenGL-Tutorial/master/HelloWindow/output.png)


## [Transformations](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/Transformations)
A simple OpenGL application, drawing a rectangle transformed by a GLMatrix with texture blending, using element buffer.  [OpenGL](https://github.com/haskell-opengl) application with [stack](https://docs.haskellstack.org/en/stable/README/) as a build system.
## Output:
![](https://raw.githubusercontent.com/madjestic/Haskell-OpenGL-Tutorial/master/Transformations/output.png)


## [Basic Hellow Window](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/tutorial00)
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/tutorial00/tutorial01.png)


## [A Hello Window, with basic input callbacks.](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/tutorial01)
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/tutorial00/tutorial01.png)


## [Drawing 2 triangles](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/tutorial02)
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/tutorial02/output.png)


## [Drawing 2 textured triangles](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/tutorial03)
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/tutorial03/output.png)


## [A colored triangle](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/tutorial04)
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/tutorial04/tutorial04_fixed.png)


## [Sugarizing the interface with polymorphic functions.](https://github.com/madjestic/Haskell-OpenGL-Tutorial/tree/master/tutorial05)
![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/tutorial05/tutorial05.png)

```haskell
module Main where

import NGL.Shape
import NGL.Rendering

main :: IO ()
main = do
     let drawables = [toDrawable Red     $ Square (-0.5, -0.5) 1.0,
                      toDrawable Green   $ Circle (0.5, 0.5) 0.5 100,
                      toDrawable Blue    $ Rect (-1.0,0.33) (0.0,0.66),
                      toDrawable White   $ Polyline [ (0.0,-0.66)
                                                     ,(0.33,-0.33)
                                                     ,(0.66,-0.66)
                                                     ,(1.0,-0.33)] 
                                                       0.01 
                     ]

     window <- createWindow "NGL is Not GLoss" (512,512)
     drawIn Default window drawables
     closeWindow window
```


