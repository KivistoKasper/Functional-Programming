nextIsGreater :: [Integer] -> [Integer]
nextIsGreater [] = [] 
nextIsGreater [_] = []
nextIsGreater list = [ x | (x,y) <- zip(init list)(tail list), x<y]