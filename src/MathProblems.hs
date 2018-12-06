module MathProblems
  ( isPrime, gcd', coprime, totientPhi
  , primeFactors

  ) where

isPrime :: Int -> Bool
isPrime n
  | n < 4     = n /= 1
  | otherwise = all ((/=0) . mod n) [2..sqrtk]
    where
      sqrtk   = (floor . sqrt . fromIntegral) n

gcd' :: Int -> Int -> Int
gcd' 0 m = m
gcd' n 0 = n
gcd' n m = gcd n (mod m n)

coprime :: Int -> Int -> Bool
-- whether two numbers gcd == 1
coprime n m = gcd n m == 1

totientPhi :: Int -> Int
-- the number of coprime integers between 1 .. n
totientPhi n = length [m | m <- [1..n], coprime n m]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors m =
  let prime = head $ dropWhile ((/= 0) . mod m) [2..m]
    in (prime:) $ primeFactors $ div m prime
