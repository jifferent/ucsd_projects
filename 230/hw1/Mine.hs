-- Pierre Fourgeaud
module Mine where 

import XMLTypes

------------------------------------------------------------------------------
-- Warm-up exercises

-- Put the solutions to these exercises here.  For each one, include at least 
-- one test case.  (These test cases don't need to be executed when the program
-- runs -- they should just be available in case someone wanted to look at them
-- from ghci, for example.)

-- 5.3
length'  = foldr (\_ x -> 1 + x) 0

-- 5.4
-- Find f1 and f2 for : f1 ( f2 (*) [1, 2, 3, 4]) 5 = [5, 10, 15, 20]
-- Assuming that f2 = map (because of the parttern matching we thought it's the best way to begin)
-- map (*) [1, 2, 3, 4] = [(*1), (*2), (*3), (*4)]
--
-- Now we have just to find 1 for
-- f1 [(*1), (*2), (*3), (*4)] 5 = [5, 10, 15, 20]

-- After some research and failure we found
-- f1 = (\x v -> map (\f -> f v) x)

-- The finale expression would be
-- (\x v -> map (\f -> f v) x) (map (*) [1, 2, 3, 4]) = [5, 10, 15, 20]


-- 5.5
-- 1.
doubleEach :: Num a => [a] -> [a]

doubleEach = map (\x -> x * 2)

doubleEach' [] = []
doubleEach' (x:xs) = x * 2 : doubleEach' xs

-- 2.
pairAndOne :: Num a => [a] -> [(a, a)]

pairAndOne = map (\x -> (x, x + 1))

pairAndOne' [] = []
pairAndOne' (x:xs) = (x, x + 1) : pairAndOne' xs

---- 3.
addEachPair :: Num a => [(a, a)] -> [a]

addEachPair = map (\x -> (fst x) + (snd x))

addEachPair' [] = []
addEachPair' (x:xs) = (fst x) + (snd x) : addEachPair' xs    

-- 5.6
maxList (x:xs)  = foldl (max) x xs

maxList' [x]     = x
maxList' (x:xs)  = max x (maxList xs)


minList (x:xs)  = foldl (min) x xs

minList' [x]       = x
minList' (x:xs)    = min x (minList xs)

-- 7.x
data Tree a         = Leaf a
                    | Branch (Tree a) (Tree a)
                    deriving Show

data InternalTree a = ILeaf 
                    | IBranch a (InternalTree a) 
                                (InternalTree a)
                    deriving Show

-- 7.1
treeFold :: (a -> a -> a) -> (b -> a) -> Tree b -> a
treeFold fn leafFn (Leaf x) = leafFn x
treeFold fn leafFn (Branch t1 t2) = fn (treeFold fn leafFn t1) (treeFold fn leafFn t2)

fringe :: Tree a -> [a]
fringe      = treeFold (++) (:[])

treeSize :: Tree a -> Integer
treeSize    = treeFold (+) (\_ -> 1)

treeHeight :: Tree a -> Integer
treeHeight  = treeFold (\x y -> 1 + max x y) (\_ -> 0)

-- We think we could redefine mapTree for example.

-- 7.2
takeTree	:: Int ->	InternalTree a -> InternalTree a
takeTree 0 _                  = ILeaf
takeTree _ ILeaf              = ILeaf
takeTree n (IBranch a t1 t2)  = IBranch a (takeTree (n-1) t1) (takeTree (n-1) t2)

takeTreeWhile :: (a -> Bool) ->	InternalTree a ->	InternalTree a
takeTreeWhile _ ILeaf              = ILeaf
takeTreeWhile fn (IBranch a t1 t2)
          | fn a      = IBranch a (takeTreeWhile fn t1) (takeTreeWhile fn t2)
          | otherwise = ILeaf

-- MAP
map' :: (a -> b) -> [a] -> [b]
map' fn [] = []
map' fn xs = foldr (\t ts -> fn t:ts) [] xs


------------------------------------------------------------------------------
-- Generic functions for transforming XML

-- Your generic functions go here...

-- Apply function on each elements of a list and pass n to the function
foldTree :: (a -> b -> c -> b) -> b -> [a] -> c -> b
foldTree fn base [] _ = base
foldTree fn base (x:xs) n = fn x (foldTree fn base xs n) n

-- Use foldTree and return a list of elements
mapTree :: (a -> b -> [a]) -> [a] -> b -> [[a]]
mapTree fn [] _ = []
mapTree fn xs n = foldTree (\t ts n -> (fn t n):ts) [] xs n

-- Flatten all the first level elements of a given list
flattenTree :: [SimpleXML] -> Integer -> [SimpleXML]
flattenTree [] _ = []
flattenTree body n = concat(mapTree transform body (n + 1))

-- Definition of an HTML BR Element
brElement = [Element "br" []]

-- Transform the initial Tree to HTML with some definitions
transform :: SimpleXML -> Integer -> [SimpleXML]
transform (Element "PLAY" body) n     = [Element "html" [Element "body" (flattenTree body n)]]
transform (Element "TITLE" body) n    = [Element ("h" ++ show(n)) (flattenTree body n)]
transform (Element "PERSONAE" body) n = (Element "h2" [PCDATA "Dramatis Personae"]):(flattenTree body n)
transform (Element "PERSONA" body) n  = (head (flattenTree body n)):brElement
transform (Element "LINE" body) n     = (head (flattenTree body n)):brElement
transform (Element "SPEAKER" body) n  = (Element "b" (flattenTree body n)):brElement
transform (Element tag body) n        = flattenTree body n
transform (PCDATA a) _                = [PCDATA a]

------------------------------------------------------------------------------
-- Formatting plays

formatPlay e          = head (transform e 0)
