--Kayla Kasprak
--CSCE 330
--Nov 9, 2016
--Exercises 4.1, 4.2, 5.3, 5.4, 5.7


--4.1: 3 points
--Using library functions, define a function halve :: [a] -> ([a],[a])
--that splits an even-lengthed list into two halves.
--For example:
-- 		>halve[1,2,3,4,5,6]
-- 		([1,2,3],[4,5,6])
halve xs = splitAt(length xs `div` 2) xs
--Output:
--Ok, modules loaded: Main.
--Main> halve [1,2,3,4]
--([1,2],[3,4])


--4.1: 3 points
--Consider a function safetail :: [a] -> [a] that behaves as the library
--function tail, except that safetail maps the empty list to itself, whereas
--tail produces an error in this case. 
--Hint: make use of the library function null
--Define safetail using:
--  (a) a conditional expression
safetaila xs = if null xs then [] else tail xs
--Output:
--Main> safetaila [1,2,3,4]
--[2,3,4]
--Main> safetaila []
--[]

--  (b) guarded equations
safetailb xs | null xs = []
 | otherwise  = tail xs
 --Output:
 --Main> safetailb []
--[]
--Main> safetailb [1,2,3,4,5]
--[2,3,4,5]

--  (c) pattern matching
safetailc [] = []
safetailc (_:xs) = xs
--Output:
--Main> safetailc []
--[]
--Main> safetailc [1,2,3]
--[2,3]


--5.3: 3 points
--A triple (x,y,z) of positive integers is pythagorean if x^2 + y^2 = z^2.
--Using list comprehension, define a funciton pyths :: Int -> [(Int, Int, Int)]
--that returns the list of all pythagorean triples whose components are at most a
--given limit.
pythagorean n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
--Output:
--Main> pythagorean 10
--[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
--Main> pythagorean 5
--[(3,4,5),(4,3,5)]


--5.4: 3 points
--A positive integer is perfect if it equals the sum of its factors, excluding
--the number itself. Using list comprehension and the fuction factors, define a
--function perfects :: Int -> Int -> [Int] that returns the list of all perfect
--numbers up to a given limit. 
--x such that x is drawn from 1 to n, 
--init: returns all elements of the list except the last
--sum of the factors must equal x
--factors function given on pg 39
factors n = [x | x <- [1..n], n `mod` x == 0]
perfects n = [x | x <- [1..n], sum(init(factors x))==x]
--Output:
--Ok, modules loaded: Main.
 --[6,28,496]

--5.7: 6 points
--The scalar product of two lists of integers xs and ys of length n is given by the
--sum of the products of corresponding integers. In a similar manner to the function
--chisqr, show how a list comprehension can be used to define a function
--scalarproduct :: [Int] -> [Int] -> Int that returns the scalar product of two lists
--zip (pg 40) pairs successive elements from two existing lists.
scalarproduct xs ys = sum[x*y | (x,y) <- zip xs ys]
--Output:
--[1 of 1] Compiling Main             ( hw9.hs, interpreted )
--Ok, modules loaded: Main.
--Main> scalarproduct [1,2,3] [4,5,6]
--32
--Main> scalarproduct [1,1,1] [7,2,3]
--12


