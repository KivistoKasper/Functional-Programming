gap :: (Char, Char) -> Integer -> String -> Integer
gap _ _ [] = 0
gap _ _ [_] = 0
gap (c1,c2) g s 
    | length s <= fromIntegral g = 0
    | head s == c1 && (s !! (fromIntegral g +1)) == c2 = 1 + gap (c1, c2) g (tail s)
    | otherwise = gap (c1, c2) g (tail s)