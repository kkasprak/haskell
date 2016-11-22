--Kayla Kasprak
--Chapter 7
--HW11 (PR8)

import Data.Char (ord, chr)

--Exercise 8: 10 points
--Modify the string transmitter program to detect simple transmission errors
--using parity bits. That is, each eight-bit binary number produced during
--encoding is extended with a parity bit, set to one if the number contains
--an odd number of ones, and to zero otherwise. In turn, each resulting ninebit
--binary number consumed during decoding is checked to ensure that its
--parity bit is correct, with the parity bit being discarded if this is the case,
--and a parity error reported otherwise.
--Hint: the library function error :: String -> a terminates evaluation and
--displays the given string as an error message.

type Bit = Int
--A binary number, represented as a list of bits, can be converted into an integer 
--by evaluating the required weighted sum:
bin2int :: [Bit]-> Int
--bin2int bits = sum [w*b | (w,b)  <- zip weights bits]
--               where weights = iterate (*2) 1
--the expression iterate (∗2) 1 used within the definition of bin2int produces
--the list of weights [1, 2, 4, 8, · · ·].
--Can be rewritten as:
bin2int = foldr (\ x y -> x + 2 * y) 0
{-GHCI Output:
*Main> bin2int[1,0,1,1]
13
-}

--Int to Binary using recursion
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

--truncates or extends binary # to make it 8 bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8  (bits ++ repeat 0)

--TRANSMISSION:
--parity bit if sum of bits is odd = 1, else = 0
parity :: [Bit] -> Bit
parity bits | odd (sum bits) = 1
            | otherwise = 0

--add parity bit to beginning of string
addParity :: [Bit] -> [Bit]
addParity bits = parity bits : bits

--check for prescence of parity bit (error)
--if not, take only the tail
check :: [Bit] -> [Bit]
check (x:xs) | x == parity xs=xs
             | otherwise = error "parity bit error detected"

--Convert each character into a Unicode number
--convert each number into 8 bit binary number
--concatenate each of these together to product a list of bits
--(use map and composition)
--Concatenate parity bit to beginning to detect errors
encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)
{-GHCI Output:
*Main> encode "abc"
[1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
-}

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . check) . chop9
{-GHCI Output:
*Main> decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
"abc"
-}
{-GHCI Output for 1 bit error:
*Main> encode "abc"
[1,1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0]
*Main> decode [1,1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0,1]
"abc*** Exception: parity bit error detected
CallStack (from HasCallStack):
  error, called at 11.hs:56:28 in main:Main}
-}

--Transmit simulates the transmission of a string of charactersas lists of bits
transmit :: String -> String
transmit = decode . channel . encode
channel :: [Bit] -> [Bit]
channel = id
{-GHCI Output:
*Main> transmit "higher-order functions are easy"
"higher-order functions are easy"
-}


--Exercise 9: 5 points
--Test your new string transmitter program from the previous exercise using
--a faulty communication channel that forgets the first bit, which can be
--modelled using the tail function on lists of bits.

--First bit is forgotten (taking only the tail), which results in an error
--when calling faultyTransmit
faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

faultyTransmit :: String -> String
faultyTransmit = decode . faultyChannel . encode
{- GHCI Output:
*Main> transmit "higher-order functions are easy"
"higher-order functions are easy"
*Main> faultyTransmit "higher-order functions are easy"
"*** Exception: parity bit error detected
CallStack (from HasCallStack):
  error, called at 11.hs:56:28 in main:Main
-}









