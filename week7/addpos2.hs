addpos :: [Integer] -> Integer
addpos [] = 0
addpos lst = 
    let pos = filter(>=0) lst 
        cut = takeWhile(>0) pos
    in sum cut