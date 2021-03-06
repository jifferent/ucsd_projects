---
title: Homework #3, Due Friday, February 24th
---

Preliminaries
=============

Before starting this part of the assignment:

1. Install `parsec3` via the command `cabal install parsec3`
2. Learn to read the [documentation](http://hackage.haskell.org)
3. Download the test files 
   [test.imp](/static/test.imp),
   [fact.imp](/static/fact.imp), 
   [abs.imp](/static/abs.imp), 
   [times.imp](/static/times.imp).


Submission Instructions
=======================

To complete this homework, download [this file as plain text](hw3.lhs) and
answer each question, filling in code where noted (where it says "TBD"). 
Your code must typecheck against the given type signatures.
Feel free to add your own tests to this file to exercise the functions
you write.  Submit your homework by sending this file, filled in
appropriately, to `cse230@goto.ucsd.edu` with the subject "HW3"; you
will receive a confirmation email after submitting.  Please note that
this address is unmonitored; if you have any questions about the
assignment, email Pat at `prondon@cs.ucsd.edu`.

> {-# LANGUAGE TypeSynonymInstances #-}
> module Hw3 where
> import Fal hiding (between, pball, walls, paddle)
> import Animation (picToGraphic)
> import qualified SOE as G
> import Picture
> import Data.Map
> import Control.Monad.State hiding (when)
> import Text.Parsec hiding (State, between)
> import Text.Parsec.Combinator hiding (between)
> import Text.Parsec.Char
> import Text.Parsec.String

Problem 1 : Pong
================

For the first problem, extend the paddleball game we saw in class to a two
player [Pong](http://en.wikipedia.org/wiki/Pong).

![Pong ScreenShot](/static/pong.png)

Both players start with `0` points, and whenever a player misses the ball,
the other player gets a point. When the game begins, and after each player 
wins a point, the score is shown and the game must continue after a key is 
pressed. The game must continue till a player reaches

> maxscore = 5

at which point she is declared the winner. The top level game is issued by
the function

> playPong = reactimate "pong" $ pong 0 0 2.0

which renders the behavior

> pong ::  Integer -> Integer -> Float -> Behavior G.Graphic
> pong p1score p2score vel =
>   if p1score == maxscore then
>     lift0 $ G.text (0, 0) "Player 1 wins!"
>   else if p2score == maxscore then
>     lift0 $ G.text (0, 0) "Player 2 wins!"
>   else
>     lift0 (G.text (0, 0) $ show p1score ++ " vs. " ++ show p2score)
>     `untilB` key ->> play p1score p2score vel


Your task is to fill in the implementation of following function

> play ::  Integer -> Integer -> Float -> Behavior G.Graphic
> play p1score p2score vel =  (lift1 picToGraphic $ display xpos ypos) `untilB` (p1lose .|. p2lose)
>   where xpos = integral xvel
>         ypos = integral yvel
>         xvel = vel `stepAccum` xhit ->> negate
>         yvel = vel `stepAccum` yhit ->> negate
>         xhit = when (xpos >*  2 ||* xpos <* -2)
>         yhit = when ((ypos      `between` (-2.0,-1.5) &&*
>                       p1input   `between` (xpos-0.25, xpos+0.25))
>                     ||* (ypos      `between` (1.5, 2.0) &&*
>                          p2input   `between` (xpos-0.25, xpos+0.25)))
>         p1lose = when (ypos <* -2.5) ->> pong p1score (p2score+1) vel
>         p2lose = when (ypos >* 2.5) ->> pong (p1score+1) p2score vel
> 
> x `between` (a, b) = x >* a &&* x <* b

> display xpos ypos = walls `over` paddle1 `over` paddle2 `over` paint yellow (translate (xpos, ypos) ball)
>   where ball  = ell 0.2 0.2

Use the same conditions as in `paddleball` to determine when the ball has
hit the paddle. The ball can be said to have missed player 1's paddle (ie
player 2 scores a point) when the ball's y-coordinate  

~~~~~~{.haskell}
ypos <* -2.5
~~~~~~

dually, player 1 scores a point when

~~~~~~{.haskell}
ypos >* 2.5
~~~~~~

You may use the following behaviors to render the wall,

> walls = left `over` right
>   where left  = paint blue $ translate (-2.2,0) (rec 0.05 3.4)
>         right = paint blue $ translate ( 2.2,0) (rec 0.05 3.4)

the paddles for each player,

> paddle1 ::  Behavior Picture
> paddle1 = paddle (-1.7) red p1input
>
> paddle2 ::  Behavior Picture
> paddle2 = paddle 1.7 green p2input
>
> paddle :: Behavior Float-> Behavior Color-> Behavior Float-> Behavior Picture
> paddle y color pos = paint color $ translate (pos, y) (rec 0.5 0.05)

The positions of the paddles of each player are given by the following
behaviors

> p1input ::  Behavior Float
> p1input = keyboardPos
> p2input ::  Behavior Float
> p2input = fst mouse

which are generated thus (you can ignore this if you are not curious...)

> keyUpE k = Event (\(uas,_) -> Prelude.map getkey uas)
>   where getkey (Just (G.Key k' False)) | k' == k = Just ()
>         getkey _                               = Nothing
>         
> kbSpeed = 2.5
> 
> keyboardVel = lift0 0 `switch` key =>> \k ->
>   case k of
>     'a' -> lift0 (-kbSpeed) `untilB` (keyUpE 'a') ->> lift0 0
>     'd' -> lift0 kbSpeed `untilB` (keyUpE 'd') ->> lift0 0
>     _   -> lift0 0
> 
> keyboardPos = integral keyboardVel


Problem 2: An Interpreter for WHILE 
===================================

Next, you will use monads to build an evaluator for
a simple *WHILE* language. In this language, we will
represent different program variables as 

> type Variable = String

Programs in the language are simply values of the type

> data Statement =
>     Assign Variable Expression          -- x = e
>   | If Expression Statement Statement   -- if (e) {s1} else {s2}
>   | While Expression Statement          -- while (e) {s}
>   | Sequence Statement Statement        -- s1; s2
>   | Skip                                -- no-op
>   deriving (Show)

where expressions are variables, constants or 
binary operators applied to sub-expressions

> data Expression =
>     Var Variable                        -- x
>   | Val Value                           -- v 
>   | Op  Bop Expression Expression
>   deriving (Show)

and binary operators are simply two-ary functions

> data Bop = 
>     Plus     -- +  :: Int  -> Int  -> Int
>   | Minus    -- -  :: Int  -> Int  -> Int
>   | Times    -- *  :: Int  -> Int  -> Int
>   | Divide   -- /  :: Int  -> Int  -> Int
>   | Gt       -- >  :: Int -> Int -> Bool 
>   | Ge       -- >= :: Int -> Int -> Bool
>   | Lt       -- <  :: Int -> Int -> Bool
>   | Le       -- <= :: Int -> Int -> Bool
>   deriving (Show)

> data Value =
>     IntVal Int
>   | BoolVal Bool
>   deriving (Show)

We will represent the *store* i.e. the machine's memory, as an associative
map from `Variable` to `Value` 

> type Store = Map Variable Value

**Note:** we don't have exceptions (yet), so if a variable
is not found (eg because it is not initialized) simply return 
the value `0`. In future assignments, we will add this as a 
case where exceptions are thrown (the other case being type errors.)

We will use the standard library's `State` 
[monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
to represent the world-transformer.
Intuitively, `State s a` is equivalent to the world-transformer 
`s -> (a, s)`. See the above documentation for more details. 
You can ignore the bits about `StateT` for now.

Expression Evaluator
--------------------

First, write a function 

> evalE :: Expression -> State Store Value

that takes as input an expression and returns a world-transformer that
returns a value. Yes, right now, the transformer doesnt really transform
the world, but we will use the monad nevertheless as later, the world may
change, when we add exceptions and such.

**Hint:** The value `get` is of type `State Store Store`. Thus, to extract 
the value of the "current store" in a variable `s` use `s <- get`.

> evalE (Var x)      = state ( \s -> (findWithDefault (IntVal 0) x s, s))
> evalE (Val v)      = state ( \s -> (v, s))
> evalE (Op o e1 e2) = state ( \s -> (applyBop o (e1' s) (e2' s), s))
>     where e1' s = evalState (evalE e1) s
>           e2' s = evalState (evalE e2) s

> applyBop :: Bop -> Value -> Value -> Value
> applyBop Plus (IntVal x) (IntVal y) = IntVal (x + y)
> applyBop Minus (IntVal x) (IntVal y) = IntVal (x - y)
> applyBop Times (IntVal x) (IntVal y) = IntVal (x * y)
> applyBop Divide (IntVal x) (IntVal y) = IntVal (x `div` y)
> applyBop Gt (IntVal x) (IntVal y) = BoolVal (x > y)
> applyBop Ge (IntVal x) (IntVal y) = BoolVal (x >= y)
> applyBop Lt (IntVal x) (IntVal y) = BoolVal (x < y)
> applyBop Le (IntVal x) (IntVal y) = BoolVal (x <= y)

Statement Evaluator
-------------------

Next, write a function

> evalS :: Statement -> State Store ()

that takes as input a statement and returns a world-transformer that
returns a unit. Here, the world-transformer should in fact update the input
store appropriately with the assignments executed in the course of
evaluating the `Statement`.

**Hint:** The value `put` is of type `Store -> State Store ()`. 
Thus, to "update" the value of the store with the new store `s'` 
do `put s`.

> evalS w@(While e s)    = put (result e')
>     where result (BoolVal b)
>             | b == True      = l
>             | otherwise      = empty
>           e'  = evalState (evalE e) empty
>           l   = snd (runState (evalS w) empty)
> evalS Skip             = put empty
> evalS (Sequence s1 s2) = do evalS s1 >> evalS s2
>                             return ()
> evalS (Assign x e )    = put (singleton x (s))
>     where s = evalState (evalE e) empty
> evalS (If e s1 s2)     = put (result e')
>     where result (BoolVal b)
>             | b == True      = snd (runState (evalS s1) empty)
>             | b == False     = snd (runState (evalS s2) empty)
>           result _  = empty
>           e' = evalState (evalE e) empty

In the `If` case, if `e` evaluates to a non-boolean value, just skip both
the branches. (We will convert it into a type error in the next homework.)
Finally, write a function 

> execS :: Statement -> Store -> Store
> execS = (\e s -> execState (evalS e) s)

such that `execS stmt store` returns the new `Store` that results
from evaluating the command `stmt` from the world `store`. 
**Hint:** You may want to use the library function 

~~~~~{.haskell}
execState :: State s a -> s -> s
~~~~~

When you are done with the above, the following function will 
"run" a statement starting with the `empty` store (where no 
variable is initialized). Running the program should print 
the value of all variables at the end of execution.

> run :: Statement -> IO ()
> run stmt = do putStrLn "Output Store:" 
>               putStrLn $ show $ execS stmt empty

Here are a few "tests" that you can use to check your implementation.

> w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

> w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

As you can see, it is rather tedious to write the above tests! They
correspond to the code in the files `test.imp` and `fact.imp`. When you are
done, you should get

~~~~~{.haskell}
ghci> run w_test
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]

ghci> run w_fact
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~
