--Kayla Kasprak
--Chapter 2 Exercises (all)
--Chapter 3 Ex 3


--double x = x+x
quadruple x = double(double x)

factorial n = product[1..n]
average ns = sum ns `div` length ns



n = a `div` length xs
 where
  a = 10
  xs = [1,2,3,4,5]

--last xs = xs!!(length-1)

--last xs = head(reverse xs)

remLast xs = take (length xs -1) xs
removeLast xs = reverse (drop 1(reverse xs))


--CHAPTER 3
second xs = head (tail xs)

swap (x , y) = (y, x )

pair x y = (x , y)

double x = x * 2

palindrome xs = reverse xs == xs

twice f x = f (f x)



