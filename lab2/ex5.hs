-- import Data.List
-- length [(a, b, c) | a<-[1..100], b<-[1..a], c<-[1..100],a^2+b^2==c^2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

-- isPrime2 n = any (==n) (take n primes)
