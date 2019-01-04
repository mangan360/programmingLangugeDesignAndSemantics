module Main where

import Data.Char (digitToInt, intToDigit)

-- | Once you have a function that computes 'carrying' properly, you're pretty
-- much done.
main :: IO ()
main = undefined


type Base = Int
type Digits = [Int]
type Carry = Int

-- | Takes a function, a base, an old carry value, and a nonempty list of
-- numbers, and uses it to calculate a new value and carry. We can use this for
-- both addition and multiplication.
computeCarry :: (Int -> Int -> Int) -> Base -> Int -> [Int] -> (Int, Carry)
computeCarry op b oldCarry ns =
  ((oldCarry + foldl1 op ns) `mod` b, (oldCarry + foldl1 op ns) `div` b)


plus :: Base -> Digits -> Digits -> Digits
plus b ns ms = loop 0 ns ms
  where
    -- a few annoying trivial cases to deal with 
    loop carry [] []
      | carry == 0 = []
      | otherwise  = [carry]
    
    loop carry [] ms =
      loop carry [0] ms

    loop carry ns [] =
      loop carry ns [0]

    loop carry (n:ns) (m:ms) = r : loop carry' ns ms
      where (r, carry') = computeCarry (+) b carry [n, m]


timesConstant :: Base -> Int -> Digits -> Digits
timesConstant b n ms = loop 0 ms
  where
    loop carry []
      | carry == 0 = []
      | otherwise  = [carry]
    
    loop carry (m:ms) = r : loop carry' ms
      where (r, carry') = computeCarry (*) b carry [n, m]


times :: Base -> Digits -> Digits -> Digits
times _ _  []     = []
times b ns (m:ms) =
  plus b (timesConstant b m ns) (0 : times b ns ms)


reverseMap :: (a -> b) -> [a] -> [b]
reverseMap f = foldl (\l x -> f x : l) []

strToDigits :: String -> Digits
strToDigits = reverseMap digitToInt

digitsToStr :: Digits -> String
digitsToStr = reverseMap intToDigit
