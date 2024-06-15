import Data.List
sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDesc2 :: Ord a => [a] -> [a]
sortDesc2 xs = reverse (sort xs)