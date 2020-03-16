module Main where

-- double function
double :: Integer -> Integer
double x = x * 2

-- basic factorial function using guards
fact :: Integer -> Integer
fact x 
    | x > 1 = x * fact (x - 1)
    | otherwise = 1

-- basic fibonacci fucntion
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib  (x -  2)

-- improved fibonacci function using tuples
fibTuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
fibTuple (x, y, 0) = (x, y, 0)
fibTuple (x, y, index) = fibTuple(y, x + y, index - 1)

fibResult :: (Integer, Integer, Integer) -> Integer
fibResult (x, y, z) = x

fib2 :: Integer -> Integer
fib2 x = fibResult (fibTuple(0, 1, x))

-- another fibonacci function using .
fibNextPair :: (Integer, Integer) -> (Integer, Integer)
fibNextPair (x, y) = (y, x+y)

fibNthPair :: Integer -> (Integer, Integer)
fibNthPair 1 = (1, 1)
fibNthPair n = fibNextPair (fibNthPair (n - 1))

fib3 :: Integer -> Integer
fib3 = fst . fibNthPair

size :: [Char] -> Integer
size [] = 0
size (h:t) = 1 + size t

prod [] = 1
prod (h:t) = h * prod t

-- allEven book
allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (h:t) = if even h then h:allEven t else allEven t

-- How many different ways can you find to write allEven?

-- Write a function that takes a list and returns the same list in reverse.

-- reverse [1, 2, 3, 4, 5, 6, 7, 8]

{- Write a function that builds two-tuples with all possible combinations
of two of the colors black, white, blue, yellow, and red. Note
that you should include only one of (black, blue) and (blue, black). -}

colors = ["black", "white", "blue", "yellow", "red"]
colorssorted = [(a, b) | a <- colors, b <- colors]

{- Write a list comprehension to build a childhood multiplication
table. The table would be a list of three-tuples where the first two
are integers from 1–12 and the third is the product of the first two. -}

tables = [(a, b, c) | a <- [1..12], b <- [1..12], let c = a * b]

{- Solve the map-coloring problem (Section 4.2, Map Coloring, on
page 101) using Haskell. -}

main = print (fib3 100)