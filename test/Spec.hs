module Main where

import Text.Printf
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import ListProblems
import Data.Char

main :: IO ()
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

prop_last'_head_drop_len s = not (null s) ==> (head $ drop (length s - 1) s) == last' s
  where _ = s :: [Int]

prop_elemBeforeLast_acc s = not (null s || length s == 1) ==> s!!(length s - 2) == elemBeforeLast s
  where _ = s :: [Int]

prop_len'_acc s = length s == len' s
  where _ = s :: [Int]

prop_len'_idempotent s = len' s == len' s
  where _ = s :: [Int]

prop_elemAt s n = not (null s || n <= 1 || n > length s) ==> elemAt s n == s!!(n - 1)
  where _ = s :: [Int]

prop_rev_works s = reverse s == rev' s
  where _ = s :: [Int]

prop_rev_idempotent s = rev' s == rev' s
  where _ = s :: [Int]

prop_isPalindrome_works s = (reverse s == s) ==> isPalindrome s
  where _ = s :: [Int]

prop_isPalindrome_works_inverse s = reverse s /= s ==> not (isPalindrome s)
  where _ = s :: [Int]

prop_dropAt_first_equals_tail s = length s > 1 ==> tail s == l
  where _      = s :: String
        (_, l) = dropAt 1 s

prop_transform_idemp s = t == t
  where _ = s :: String
        t = transform s

prop_transform_works s = transform s == concatMap ($s) [drop (halflen s)
                                                      , take (halflen s)]
  where 
    _       = s :: [Char]
    halflen = flip quot 2 . length

prop_insertAt_1_same_as_prepend s = insertAt s "abcde" 1 == s:"abcde"
    where _ = s :: Char

tests = [("prop_last'", quickCheck prop_last'_head_drop_len)
  , ("prop_elemBeforeLast", quickCheck prop_elemBeforeLast_acc)
  , ("prop_len_acc", quickCheck prop_len'_acc)
  , ("prop_len_idempotent", quickCheck prop_len'_idempotent)
  , ("prop_elemAt", quickCheck prop_elemAt)
  , ("prop_rev_works", quickCheck prop_rev_works)
  , ("prop_rev_idempotent", quickCheck prop_rev_idempotent) 
  , ("prop_isPalindrome_works"
  ,  quickCheck prop_isPalindrome_works)
  , ("prop_isPalindrome_works_inverse"
  , quickCheck prop_isPalindrome_works_inverse)
  , ("prop_dropAt_first_equals_tail", quickCheck prop_dropAt_first_equals_tail)
  , ("prop_transform_idemp", quickCheck prop_transform_idemp)
  , ("prop_transform_works", quickCheck prop_transform_works)
  , ("prop_insertAt_1_same_as_prepend", quickCheck prop_insertAt_1_same_as_prepend)
  ]
