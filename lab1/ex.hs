f :: Int -> Int
f x = if x >= 0 then x else (-x)
isDigit :: Char -> Bool
isDigit a = case a of
            '0' -> True
            '1' -> True
            '2' -> True
            '3' -> True
            '4' -> True
            '5' -> True

not' :: Bool -> Bool
not' True = False
not' False = True