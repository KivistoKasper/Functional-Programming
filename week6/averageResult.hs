averageResult :: [ Integer -> Integer ] -> ([Integer] -> Integer)
averageResult _ [] = error "At least one function required"
averageResult [] _ = error "At least one function required"
averageResult funcs ints 
    | length funcs /= length ints = error "Lists have different lengths" 
    | otherwise =
      let nums = [ f i | (f,i) <- zip funcs ints]
      in sum nums `div` fromIntegral (length nums)