addpos :: [Integer] -> Integer
addpos lst = foldr add 0 lst
    where add elem collect | elem > 0 = collect + elem
                           | elem == 0 = 0
                           | otherwise = collect