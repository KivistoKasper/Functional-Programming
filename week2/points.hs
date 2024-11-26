points :: Integer -> [(Integer, Integer)]
points d = [ (x,y) | x <- [-d..d], y<-[-d..d], abs(x)+abs(y) <= d]
