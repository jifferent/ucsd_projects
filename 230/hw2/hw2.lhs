---
title: Homework #2, Due February 10th
---

This week's homework is presented as a literate Haskell file,
just like the lectures. This means that every line beginning with
`>` is interpreted as Haskell code by the compiler, while every other
line is ignored. (Think of this as the comments and code being reversed
from what they usually are.)
You can load this file into `ghci` and compile it with `ghc`
just like any other Haskell file, so long as you remember to save
it with a `.lhs` suffix.

To complete this homework, download [this file as plain text](hw2.lhs) and
answer each question, filling in code where
noted (some questions ask for explanations in addition to or instead
of code).
Your code must typecheck against the given type signatures.
Feel free to add your own tests to this file to exercise the functions
you write.  Submit your homework by sending this file, filled in
appropriately, to `cse230@goto.ucsd.edu` with the subject "HW2"; you
will receive a confirmation email after submitting.  Please note that
this address is unmonitored; if you have any questions about the
assignment, email Pat at `prondon@cs.ucsd.edu`.

This homework requires the graphics libraries from
The Haskell School of Expression:

> import Animation hiding (planets, translate)
> import Picture
> import Control.Applicative

Part 0: All About You
---------------------

Tell us your name, email and student ID, by replacing the respective
strings below

> myName  = "Pierre-Louis Gottfrois"
> myEmail = "pierrelouis.gottfrois@gmail.com"
> mySID   = "U06064374"

I worked with Pierre Fourgeaud.

Part 1: All About `foldl`
-------------------------

Define the following functions by filling in the "error" portion:

1. Describe `foldl` and give an implementation:

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl f b [] = b
> myFoldl f b (x:xs) = myFoldl f (f b x) xs

Foldl processes the list from left to right starting with the first element and proceeding to the last one step by step.

2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

> myReverse :: [a] -> [a]
> myReverse xs = foldl (\a x -> x:a) [] xs

3. Define `foldr` in terms of `foldl`:

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr f b xs = foldl (\g a x -> g (f a x)) id xs b

4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

> myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
> myFoldl2 f b xs = foldr (\a g x -> g (f x a)) id xs b

5. Try applying `foldl` to a gigantic list. Why is it so slow?
   Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
   instead; can you explain why it's faster?

seq is used in foldl, it's usually introduced to improve performance by avoiding unneeded laziness.

Part 2: Binary Search Trees
---------------------------

Suppose we have the following type of binary search trees:

> data BST k v = Emp 
>              | Bind k v (BST k v) (BST k v) 
>              deriving (Show)

Define a `delete` function for BSTs of this type:

> putLeft Emp Emp = Emp
> putLeft Emp a   = a
> putLeft a Emp   = a
> putLeft a (Bind k v l r) = (Bind k v (putLeft a l) r)

> delete :: (Ord k) => k -> BST k v -> BST k v
> delete k t
>        = case t of
>           Emp -> Emp
>           Bind k' v' l r
>             | k == k'      -> putLeft l r
>             | k <  k'      -> Bind k' v' (delete k l) r 
>             | otherwise    -> Bind k' v' l (delete k r)

Part 3: Animation
-----------------

This part of the homework constructs an animation of a model
solar system.
We begin by defining various helpful functions:

> translate :: (Float, Float) -> Picture -> Picture
> translate v p =
>     case p of
>       Region c r -> Region c (Translate v r)
>       p1 `Over` p2 -> (translate v p1) `Over` (translate v p2)
>       EmptyPic -> EmptyPic

> -- Translate a picture behavior by a given vector behavior
> translateB :: (Behavior Float, Behavior Float) -> Behavior Picture -> Behavior Picture
> translateB (x,y) p = lift2 translate (zipB (x,y)) p

> -- Convert a pair of behaviors into a pair behavior
> zipB :: (Behavior a, Behavior b) -> Behavior (a,b)
> zipB (Beh b1, Beh b2) = Beh (\t -> (b1 t, b2 t))

> -- Construct a circle behavior
> circ :: Behavior Float -> Behavior Shape
> circ r = ell r r

> sun :: Behavior Picture
> sun = reg (lift0 Yellow) (shape (circ 1))

The following define the main action of the solar system simulator.
You'll want to replace the right-hand side of `planets` with your
solar system.

> mercury :: Behavior Picture
> mercury = reg (pure Red) (shape (circ 0.171))

> venus :: Behavior Picture
> venus = reg (pure Yellow) (shape (circ 0.428))

> earth :: Behavior Picture
> earth = reg (pure Blue) (shape (circ 0.457))

> mars :: Behavior Picture
> mars = reg (pure Red) (shape (circ 0.228))

> moon :: Behavior Picture
> moon = reg (pure White) (shape (circ 0.124))

> planets :: Behavior Picture
> planets = overMany [p1,p2,p3,p4]
>   where p1 = orbit mercury  sun 3.146   2.0   0.2   2.0
>         p2 = orbit venus    sun 0.811   2.4   0.6   2.0
>         p3 = orbit earth    sun 0.5     2.8   1.0   2.0
>         p4 = orbit mars     sun 0.272   3.0   1.2   2.0

> main :: IO()
> main = 
>   do animateB "Solar system" planets

Before starting the exercise proper, let's make our lives easier.
You can avoid a lot of tedious "liftn" operations in your code if you
make the Behavior type a member of the Applicative typeclass. This may
require providing additional definitions not explicitly mentioned
here. You should verify that your definition of the applicative
instance has the required properties (but don't need to turn in a
proof).
If you don't understand the above, some good references are
Chapter 18 of The Haskell School of Expression
and the
[section on applicative functors](http://en.wikibooks.org/wiki/Haskell/Applicative_Functors)
in the Haskell wikibook.

> instance Functor Behavior where
>   fmap f (Beh a) = Beh $ \t -> f (a t)
> 
> instance Applicative Behavior where
>   pure x                 = Beh $ \t -> x
>   (<*>) (Beh ab) (Beh a) = Beh $ \t -> (ab t) (a t)

Next, use the provided function translateB to write a function

> class Deformable a where
>   stretch     :: Float -> a -> a

> instance Deformable Shape where
>   stretch x (Ellipse a b) = Ellipse (a*x) (b*x)

> instance Deformable Region where
>   stretch x  (Shape sh)   = Shape (stretch x  sh)

> instance Deformable Picture where
>   stretch x (Region c r)    = Region c (stretch x r)
>   stretch x (p1 `Over` p2)  = stretch x p1 `Over` stretch x p2
>   stretch x EmptyPic        = EmptyPic

> instance Deformable a => Deformable (Behavior a) where
>   stretch x  (Beh b) = Beh (stretch x . b)

> cond' :: Float -> Behavior Picture -> Behavior Picture -> Behavior Picture
> cond' freq a b = cond ((-sin (time * pure freq)) >* 0) a b

> orbit :: Behavior Picture -- the satellite
>       -> Behavior Picture -- the fixed body
>       -> Float            -- the frequency of the orbit
>       -> Float            -- the x-radius of the orbit
>       -> Float            -- the y-radius of the orbit
>       -> Float            -- the growth and shrink factor of the planet
>       -> Behavior Picture

> orbit sat fix freq xrad yrad f = cond' freq (sat' `over` fix) (fix `over` sat')
>   where xrad' = pure xrad
>         yrad' = pure yrad
>         freq' = pure freq
>         f'    = pure f
>         sat'  = translateB (xrad' * cos (time * freq'), yrad' * sin (time * freq')) $
>                 lift2 stretch (sin (time * freq' / f') - cos (time * freq' / f')) $ sat

that takes two picture behaviors and makes the first orbit around the
second at the specified distance and with the specified radii. That
is, the two pictures will be overlayed (using `over`) and, at each time
$t$, the position of the satellite will be translated by
$xradius * cos(t * frequency)$ in the $x$ dimension and by
$yradius * sin(t * frequency)$ in the $y$ dimension.

Test your function by creating another circle, `mercury`, colored red
and with radius `0.1`, and making it orbit around the sun with a
frequency of `2.0`, and with radii of `2.0` and `0.2` in the x and y axes,
respectively.

A problem you might have noticed is the overlay behavior of
planets. For this part modify orbit to put planets over or under each
other. Hint: you might find the lifted conditional `cond` from SOE
useful for this part.

Modify your functions (and write any support functions that you find
necessary) to make the orbital distances and planet sizes shrink and
grow by some factor (you can pass this factor as parameter to the
orbit function), according to how far the planets are from the
observer. For example, the earth and moon should look a little smaller
when they are going behind the sun, and the orbital distance of the
moon from the earth should be less.

Choose the scaling factor so that the solar system simulation looks
good to you.

*Optional:* Add some other planets, perhaps with their own moons. If
you like, feel free to adjust the parameters we gave above to suit
your own aesthetic or astronomical tastes. Make sure, though, that the
features requested in previous parts --- growing, shrinking,
occlusion, etc. --- remain clearly visible.

Credits
-------

Part 3 is taken from 
<a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/">
UPenn's CIS 522
</a>.
