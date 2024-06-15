isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldl1 (&&) (zipWith (<=) xs (tail xs)) -- isSortedAsc [1,2,2,3] -> True, isSortedAsc [1,2,1] -> False

everySecond :: [t] -> [t]
everySecond [] = []
everySecond (x1:x2:xs) = x1 : everySecond xs -- everySecond [1..8] -> [1,3,5,7]