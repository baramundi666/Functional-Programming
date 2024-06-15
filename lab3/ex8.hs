doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map' f xs

doubleElems1 = map' (2*)
sqrElems1    = map' (^2)
