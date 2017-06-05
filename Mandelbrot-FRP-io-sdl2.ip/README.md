An animated Mandelbrot example, using [FRP.Yampa](https://github.com/ivanperez-keera/Yampa) to handle animation loop,  SDL2 is used for windows and event callbacks, OpenGL is used for rendering.
Shader-loading is handled by [Sven Panne's code](https://github.com/haskell-opengl/GLUT/blob/master/examples/RedBook8/common/LoadShaders.hs).

Here I am trying to understand how to make a key-press-hold event should work.
I tried modifying the code according to Ivan's advice:

```haskell
data AppInput = AppInput
    { inpMousePos   :: (Double, Double)        -- ^ Current mouse position
    , inpMouseLeft  :: Maybe (Double, Double)  -- ^ Left button currently down
    , inpMouseRight :: Maybe (Double, Double)  -- ^ Right button currently down
    , inpKeyPressed :: Maybe SDL.Scancode
    , inpKeyRepeat  :: Bool
    , inpQuit       :: Bool                    -- ^ SDL's QuitEvent
    }

initAppInput :: AppInput
initAppInput = AppInput { inpMousePos   = (0, 0)
                        , inpMouseLeft  = Nothing
                        , inpMouseRight = Nothing
                        , inpKeyPressed = Nothing
                        , inpKeyRepeat  = False
                        , inpQuit       = False
                        }

-- | Filter and transform SDL events into events which are relevant to our
--   application
parseWinInput :: SF WinInput AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

-- | Compute next input
--   FIXME: I am reinventing lenses once again
nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
nextAppInput inp SDL.QuitEvent = inp { inpQuit = True }
nextAppInput inp (SDL.MouseMotionEvent ev) =
    inp { inpMousePos = (fromIntegral x, fromIntegral y) }
    where P (V2 x y) = SDL.mouseMotionEventPos ev
nextAppInput inp (SDL.KeyboardEvent ev)
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed &&
      SDL.keyboardEventRepeat    ev == True
      = inp { inpKeyPressed = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev
            , inpKeyRepeat  = True }
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed
      = inp { inpKeyPressed = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev }
    | SDL.keyboardEventKeyMotion ev == SDL.Released
      = inp { inpKeyPressed = Nothing
            , inpKeyRepeat  = False }
...

counter :: Double -> SF AppInput Double
counter k0 =
  switch sf cont
    where
         sf = proc input -> do
            timer    <- constant k0 -< ()
            zoomIn   <- trigger -< input
            returnA  -< (timer, zoomIn `tag` timer):: (Double, Event Double)
         cont x = counter (x + 0.1)

trigger :: SF AppInput (Event ())
trigger =
  proc input -> do
    -- | Desired tested behavior: tap and keep Space key tapped (assuming that SDL.keyboardEventRepeat is what we need)
    upTap   <- keyPressedRepeat (SDL.ScancodeSpace, True) -< input
    returnA -< upTap

keyPressedRepeat :: (SDL.Scancode, Bool) -> SF AppInput (Event ())
keyPressedRepeat (code, rep) =
  keyPressRepeat >>^ filterE (rep ==) >>^ tagWith ()  

keyPressRepeat :: SF AppInput (Event Bool)
keyPressRepeat = inpKeyRepeat ^>> (edge >>^ tagWith True)

```

What I am gettting is: when I hit and hold Space-key, for about a second nothing happens,
then the event seems to fire, the zoom action increments by a single step.

I want the increment to occur repeatedly while the Space-key is pressed.

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