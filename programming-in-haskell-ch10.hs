module Practice where

import Prelude hiding (and, concat, replicate, merge, elem)

{-
Kayla Kasprak
HW 10 (Pr 7)
Exercises 3,4,5 Ch.6 [H]

20 points total (all exercises from [H]):
10 points for ex.6.3 (Haskell run required),
4 points for ex.6.4 (Haskell run required),
6 points for ex.6.5  (Please follow the hint) (Haskell run required),
Up to 3 points taken away for missing examples of use (a.k.a. Haskell
runs)
Do not forget examples of use!
-}


--Exercise 3 (10 points)
-------------------------
{-Without looking at the definitions from the standard prelude, define the
following library functions using recursion.-}

-- Decide if all logical values in a list are True:
and :: [Bool] -> Bool
and[] = True --empty list
and(x:xs) = x && and xs
{-
GHCI run output:
*Practice> and[True]
True
*Practice> and[True,False]
False
*Practice>
-}

-- Concatenate a list of lists:
concat :: [[a]] -> [a]
concat[] = [] --empty list = empty list
concat(x:xs) = x ++ concat xs
{- GHCI output:
[1 of 1] Compiling Practice         ( 10.hs, interpreted )
Ok, modules loaded: Practice.
*Practice> concat[[1,2],[3,4]]
[1,2,3,4]
-}

-- Produce a list with n identical elements:

replicate :: Int -> a -> [a]
replicate 0 _ = [] --if we try to replicate anything 0 times, we have the empty list
replicate number var = var : replicate(number-1) var
{- GHCI output:
Ok, modules loaded: Practice.
*Practice> replicate 0 5
[]
*Practice> replicate 4 7
[7,7,7,7]
-}

-- Select the nth element of a list:
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x --0th element is x
(!!!) (x:xs) n = (!!!) xs (n-1)
{- GHCI output:
*Practice> (!!!) [1,2,3] 0
1
*Practice> (!!!) [7,6,9,2,1] 2
9
-}

-- Decide if a value is an element of a list:
elem :: Eq a => a -> [a] -> Bool
elem n [] = False -- any value is not an element of an empty list
elem n (x:xs) 
       | n == x = True --if n (value we're looking for) = x, then return true
       | otherwise = elem n xs
{- GHCI output:
[1 of 1] Compiling Practice         ( 10.hs, interpreted )
Ok, modules loaded: Practice.
*Practice> elem 3 [1,2,3]
True
*Practice> elem 2 []
False
*Practice> elem 7 [1,2,3]
False
-}


--Exercise 4 (4 points)
-------------------------
{- Define a recursive function merge :: Ord a ⇒ [a ] → [a ] → [a ] that
merges two sorted lists to give a single sorted list. For example:
merge [2, 5, 6] [1, 3, 4]
[1, 2, 3, 4, 5, 6]
Note: your definition should not use other functions on sorted lists such as
insert or isort , but should be defined using explicit recursion.-}
merge :: Ord a => [a] -> [a] -> [a] 
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = if y <= x 
                      then y:merge (x:xs) ys
                      else x: merge (y:ys) xs
{-GHCI output:
[1 of 1] Compiling Practice         ( 10.hs, interpreted )
Ok, modules loaded: Practice.
*Practice> merge [] [1,2]
[1,2]
*Practice> merge [1,2] []
[1,2]
*Practice> merge [1,3] [2,4]
[1,2,3,4]
*Practice>
-}

--Exercise 5 (6 points)
-------------------------
{- Using merge, define a recursive function msort :: Ord a ⇒ [a ] → [a ] that
implements merge sort, inwhich the empty list and singleton lists are already
sorted, and any other list is sorted by merging together the two lists that
result from sorting the two halves of the list separately.
Hint: first define a function halve :: [a ] → [([a ], [a ]) ] that splits a list into
two halves whose lengths differ by at most one. -}
halve :: [a] -> ([a],[a]) --splits a list into two halves whose lengths differ by <= 1
halve [] = ([],[]) -- halve an empty list, return two empty lists
halve xs = splitAt(length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort[x] = [x]
-- split xs into ys, zs and then sort recursively on each
msort xs = merge (msort a) (msort b) --merge defined above, calls msort recursively
           where (a,b) = halve xs

{-GHCI output:
main = do 
	print $ halve [1,2,6,9]
	print $ msort [1,6,3,9,4]
-}





