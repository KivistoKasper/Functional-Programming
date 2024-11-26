import Text.Read (readMaybe)

calculate :: [String] -> [String]
calculate = map processCalculation
  where
    processCalculation :: String -> String
    processCalculation input =
      case words input of
        [left, operation, right] ->
          case (readMaybe left :: Maybe Int, readMaybe right :: Maybe Int) of
            (Just x, Just y) -> case operation of
              "+" -> show (x + y)
              "-" -> show (x - y)
              "*" -> show (x * y)
              _   -> "I cannot calculate that"
            _ -> "I cannot calculate that"
        _ -> "I cannot calculate that"
