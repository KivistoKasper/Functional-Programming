distance1 :: String -> String -> Float
distance1 [] [] = 0
distance1 s1 s2 =
      let length1 = length [c | c<-s1, not (c `elem` s2) ]
          length2 = length [c | c<-s2, not (c `elem` s1) ]
          tot = fromIntegral (length s1 + length s2)
      in fromIntegral (length1 + length2 ) / tot



distance2 :: String -> String -> Float
distance2 [] [] = 0
distance2 s1 s2 =
      let length1 = length [c | c<-s1, not (c `elem` ['0'..'9']) ]
          length2 = length [c | c<-s2, not (c `elem` ['0'..'9']) ]
          tot = fromIntegral (length s1 + length s2)
      in fromIntegral (length1 + length2 ) / tot