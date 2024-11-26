speeds :: [Maybe Integer] -> [Integer] -> [Either String Integer]
speeds [] _ = error "No speed data"
speeds _ [] = error "No time data"
speeds positions timepoints
    | null positions && null timepoints         = [Left "No data provided"]
    | null positions || null timepoints         = [Left "One point only"]
    | length positions == 1 && length timepoints == 1 = [Left "One point only"]
    | length positions > length timepoints            = processPairs ++ [Left "Too many positions"]
    | length timepoints > length positions            = processPairs ++ [Left "Too many timepoints"]
    | all (== Nothing) positions                 = [Left "All data missing"]
    | otherwise                                       = processPairs
  where
    processPairs = zipWith speed (zip (init positions) (tail positions)) 
                                 (zip (init timepoints) (tail timepoints))

speed :: (Maybe Integer, Maybe Integer) -> (Integer, Integer) -> Either String Integer
speed (Nothing, _) _ = Left "Missing data"
speed (_, Nothing) _ = Left "Missing data"
speed (Just p1, Just p2) (t1, t2)
  | t2 == t1  = Left "Same time in two measurements"
  | t2 < t1   = Left "Space-time continuum broken"
  | otherwise = Right ((p2 - p1) `div` (t2 - t1))


    
-- speeds [Just 0, Just 2, Just 3] [0, 1, 2]
-- speeds [Just 0, Just 2, Nothing, Just 3] [2, 4, 6, 8, 10]