sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0
sumSqr' (x:xs) = x^2 + sum' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum = sumWith (\e -> e)
sumSqr = sumWith (\e -> e^2)
sumCube = sumWith (\e -> e^3)
sumAbs = sumWith (\e -> abs e)

listLength = sumWith (\e -> 1)

