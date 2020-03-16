module Main where

squareAll list = map square list
    where square x = x * x

-- currying: elke functie in haskell heeft maar 1 argument
-- bij functies zoals

prod x y = x * y

-- voorbeeld prod 2 4
-- wordt omgezet in
    -- (\y -> 2 * y) 4

double y = prod 2 y

-- 
triple y =  prod 3 y

-- Lazy evaluation
-- lijkt op python generator???

myRange start step = start:(myRange (start + step) step)

lazyFib x y = x:(lazyFib y (x + y))
fib = lazyFib 1 1
fibNth x = head ( drop (x - 1) (take (x) fib))

-- Write a sort that takes a list and returns a sorted list.
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | (x <= y) = x:y:ys
    | otherwise = y:(insert x ys)

insertsort :: [Ord a] => [a] -> [a]
insertsort [] = []
insertsort (first:second) = insert first (insertsort second)  

-- Write a sort that takes a list and a function that compares its two arguments and then returns a sorted list.

-- sortFunc :: (Ord a, b) => [a], b -> [a]

-- Write a Haskell function to convert a string to a number. The string should be in the form of $2,345,678.99 and can possibly have leading zeros.

-- Write a function that takes an argument x and returns a lazy sequence that has every third number, starting with x. Then, write a function that includes every fifth number, beginning with y. Combine these functions through composition to return every eighth number, beginning with x + y

-- Use a partially applied function to define a function that will return half of a number and another that will append \n to the end of any string.

main = print "day2"  