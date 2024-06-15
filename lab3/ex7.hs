onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p (x:xs)
 | p x = x : filter' p xs
 | otherwise      = filter' p xs

onlyEven1 = filter' (\x -> x `mod` 2 == 0)
onlyOdd1 = filter' (\x -> x `mod` 2 == 1)
