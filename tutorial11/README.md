Tutorial 11.

Where the usage of typeclasses is being explored.

The program defines:

class Primitive a where 
      toDrawable :: Property -> a -> Drawable
      toPoints   :: a -> Points

where the variable 'a' can be both type 'Shape' and type 'Picture'.

![](https://raw.github.com/madjestic/Haskell-OpenGL-Tutorial/master/tutorial11/main.png)