module ListProblems
  ( last', elemBeforeLast, elemAt, len'
  , rev', isPalindrome, flatten, distinct
  , packCons, rle, rleDecode, dup, dupN
  , dropEvery, splitOn, mapTuple, slice
  , rotate, dropAt, transform, insertAt
  , range', selectRandomN, lotto, randPerm
  , combinations
  ) where

import Control.Exception
import Data.Typeable
import Numeric
import Data.List
import Data.Unique
import System.Random

-- Problem 1: Find the last element of a list

last' :: [a] -> a
last' [] = errorEmptyList "last'"
last' [x] = x
last' (_:xs) = last xs

-- Problem 2: Find the second to last element of a list

elemBeforeLast :: [a] -> a
elemBeforeLast [] = errorEmptyList "elemBeforeLast"
elemBeforeLast [x] = errorWithName "elemBeforeLast" "singleton list"
elemBeforeLast (x:[y]) = x
elemBeforeLast (_:xs) = elemBeforeLast xs

-- Problem 3: Find the K-th element in a list. First elem is 1

elemAt :: [a] -> Int -> a
elemAt [] _ = errorEmptyList "elemAt"
elemAt lst idx
  | length lst < idx = errorWithName "elemAt" "Out of bounds index"
  | otherwise        = lst!!(idx - 1)


-- Problem 4: Find the length of a list

len' :: [a] -> Int
len' = foldl (\x _-> 1+x) 0

-- Problem 5: Reverse a list
rev' :: [a] -> [a]
rev' = foldl (flip (:)) []

-- Problem 6: find if list is palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = xs == rev' xs

-- Problem 7: flatten nested list

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- Problem 8: eliminate consecutive duplicates
distinct :: (Show a, Eq a) => [a] -> [a]
distinct [x] = [x]
distinct (x:xs)
  | head xs == x = distinct xs
  | otherwise    = x : distinct xs

-- Problem 9: pack consecutive elements into sublists
packCons :: (Eq a) => [a] -> [[a]]
packCons [] = []
packCons (x:xs) = (x:(takeWhile (== x) xs)) : (packCons $ dropWhile (== x) xs)

-- Problem 10: RLE of a list, using above func for compression
rle :: (Eq a) => [a] -> [(Int, a)]
rle = map ((,) <$> length <*> head) . packCons

-- Problem 11: RLE, single elements placed directly in list, no tuple
data ListItem a = Multiple Int a | Single a
  deriving (Show)

rle' :: (Eq a) => [a] -> [ListItem a]
rle' = map encoder . rle
  where
    encoder (1, x) = Single x
    encoder (n, x) = Multiple n x

-- Problem 12: Decode a RLE produced by the above func to the original string

rleDecode :: (Eq a) => [ListItem a] -> [a]
rleDecode [] = []
rleDecode (x:xs) = case x of
  Single x     -> x : rleDecode xs
  Multiple n x -> replicate n x ++ rleDecode xs

-- Problem 14: Duplicate the elements of a list

dup :: [a] -> [a]
dup = concatMap (replicate 2)
-- dup xs = concat [[x, x] | x <- xs]

-- Problem 15: replicate elements of a list n times (lol)

dupN n = concatMap (replicate n)

-- Problem 16: drop every nth element of list

dropEvery :: Int -> [a] -> [a]
dropEvery n xs = dropTill n xs
  where
    dropTill :: Int -> [a] -> [a]
    dropTill _ []     = []
    dropTill 1 (x:xs) = dropTill n xs
    dropTill k (x:xs) = x : dropTill (k-1) xs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

-- Problem 17: split on index given

splitOn :: Int -> [a] -> ([a], [a])
splitOn n xs = mapTuple ($ xs) (take n, drop n)

-- Problem 18: slice from i to j, inclusive 

slice :: Int -> Int -> [a] -> [a]
slice i j xs
  | i > j  ||
    j < 0  ||
    i < 0     = error "Invalid slice op"
  | j == 0    = take i xs
  | i == j    = return xs!!(i-1)
  | otherwise = drop (i-1) $ take j xs

{-|
  Problem 19: rotate elements of a list about an index such that
    rotate 3 [1,2,3,4] == [4,1,2,3]
    rotate 1 [1,2,3,4] == [2,3,4,1]
|-}

rotate :: Int -> [a] -> [a]
rotate 0 x = x
rotate _ [] = []
rotate n x =  drop n x ++ take n x

{-|
    Problem 20: drop element at nth index, returns tuple of
      (Maybe (a), [a])
|-}

dropAt :: Int -> [a] -> (Maybe a, [a])
dropAt _ [] = (Nothing, [])
dropAt 1 [x] = (Just x, [])
dropAt 1 (x:xs) = (Just x, xs)
dropAt n x
  | n >= length x = dropAt (n - length x) x
  | otherwise     = (Just (x!!(n-1))
                  , concatMap ($x) [slice 1 (n-1), slice (n+1) (length x)])

-- xtra - rotate a list at its midpoint
transform :: [a] -> [a]
transform [] = []
transform xs = rotate (h xs) xs
      where
        h = flip quot 2 . length

{-|
    Problem 21: insert element at nth index, starting at 1
|-}

insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _   = [x]
insertAt x xs 1   = x:xs
insertAt x xs n
  |   n > length xs
  ||  n <= 0         = error "Invalid insertAt, index is out of list bounds"
  | otherwise        = first ++ x:second
      where
        (first, second) = ((,) <$> take n <*> drop n) xs


{-|
    Problem 22: create a list of ints in a given range
    (inclusive, i.e.: range' 1 3 === [1, 2, 3])
|-}

range' :: Int -> Int -> [Int]
range' x y 
  | x > y     = error "Impossible range detected"
  | x < y     = x:range' (x + 1) y
  | x == y    = [y]
  
{-|
    Problem 23: Extract n random elements from list
|-}
-- select a random element and return a tuple with (element, rest)
randElem :: [a] -> IO (a, [a])
randElem xs = do
  r <- randomRIO (0, (length xs) - 1)
  let rest = take r xs ++ drop (r + 1) xs
  return (xs !! r, rest)

-- recursively select a random element until n is 0
selectRandomN :: Int -> [a] -> IO [a]
selectRandomN n xs
  | n < 0           = error "N must be positive"
  | n > length xs   = error "N is greater than list length"
  | n == 0          = return []
  | otherwise       = do
      (rand, rem) <- randElem xs
      rest <- selectRandomN (n - 1) rem
      return $ rand:rest

{-|
    Problem 24: Select N random numbers from 1..M
|-}

lotto :: Int -> Int -> IO [Int]
lotto n m = selectRandomN n [1..m]

{-|
    Problem 25: generate a random permutation of a list
|-}

randPerm :: [a] -> IO [a]
randPerm xs = do
  (p, _) <- randElem . permutations $ xs
  return p

{-|
    Problem 26: Generate all possible combinations
    of K elements from a list
|-}
-- cons x to all the outputs of `combinations (n-1) xs`,
-- then concat it with the output of `combinations n xs`
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations n (x:xs) = map (x:) $ combinations (n-1) xs ++ combinations n xs

--------------------------------------------------------------
-- Error code
--------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:

errorEmptyList :: String -> a
errorEmptyList fun =
  errorWithoutStackTrace (hpr_listpr_str ++ fun ++ ": empty list")

errorWithName fun msg =
  errorWithoutStackTrace (hpr_listpr_str ++ fun ++ ": " ++ msg)

hpr_listpr_str :: String
hpr_listpr_str = "HProblems.ListProblems."
