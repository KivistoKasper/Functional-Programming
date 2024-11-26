charsDivisibleBy :: Integer -> [Char]
charsDivisibleBy n = [ a | (a, i) <- zip ['a'..'z'] [1..26], i `mod` n == 0 ]


charsProductOf :: [Integer] -> [Char]
charsProductOf [] = []
charsProductOf [_] = []
charsProductOf ns = [ a | (a, i) <- zip ['a'..'z'] [1..26], i `elem` product]
  where
    product = [x * y | x <- ns, y <- ns, x /= y, x * y <= 26]