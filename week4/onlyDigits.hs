onlyDigits :: String -> Bool
onlyDigits "" = False
onlyDigits [x] = isDigit x
onlyDigits (x:xs)  
  | isDigit x = onlyDigits xs
  | otherwise = False


isDigit :: Char -> Bool
isDigit c
  | c `elem` ['0'..'9'] = True
  | otherwise = False