clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters f d ss = map (\s -> filter (\s' -> f s s' <= d) ss) ss
--clusters distance1 0.3 ["aaabc", "aabdd", "a", "aa", "abdd", "bcbcb", "", "abcdefghij"]
--clusters distance2 0.2 ["123a","456789b","45","abc", "ab1", "a12", "abcdefghij"]

distance1 :: String -> String -> Float
distance1 [] [] = 0
distance1 s1 s2 =
      let length1 = length [c | c<-s1, not (c `elem` s2) ]
          length2 = length [c | c<-s2, not (c `elem` s1) ]
          tot = fromIntegral (length s1 + length s2)
      in fromIntegral (length1 + length2 ) / tot
--( (count of how many of the characters in s1 do not appear in s2 + 
--(count of how many of the characters in s2 do not appear in s1) ) /
-- ( (length of s1) + (length of s2) )
-- EXAMPLE: “aaabc” and “aabdd” with this function is (1 + 2) / (5 + 5)


distance2 :: String -> String -> Float
distance2 [] [] = 0
distance2 s1 s2 =
      let length1 = length [c | c<-s1, not (c `elem` ['0'..'9']) ]
          length2 = length [c | c<-s2, not (c `elem` ['0'..'9']) ]
          tot = fromIntegral (length s1 + length s2)
      in fromIntegral (length1 + length2 ) / tot
--( (count of characters in s1 that are other than any of ‘0’..‘9’) + 
--(count of characters in s2 that are other that any of ‘0’..‘9’) ) / 
--( (length of s1) + (length of s2) )
--EXAMPLE: “xy765” and “abc2311” with this function is (2 + 3) / (5 + 7)