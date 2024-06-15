f8 = \x -> ((\y -> 2*y^3*(y+1)) . sqrt) x

f9 = \x -> case x of 1 -> 3 
                     _ -> 0