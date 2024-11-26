headOrLast :: [String] -> Char -> [String]
headOrLast list c = [ x | x <- list, not (null x), head(x) == c || last(x)==c]
