import Data.List
isPalindrome :: [Char] -> Bool

isPalindrome w = w == reverse w

--let xs = [1..5]

--getElementAtIdx :: [Int] -> Int -> Int
getElementAtIdx xs x = if x<length xs && -1<x
                        then head (drop x xs)
                        else -99