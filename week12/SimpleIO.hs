import qualified Data.Map

initialstate :: String
initialstate = ""

encode :: Int -> String -> String
encode shift msg = map (charmap Data.Map.!) msg
  where charlist = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
        listlength = length charlist
        shiftedlist = take listlength (drop (shift `mod` listlength) (cycle charlist))
        charmap = Data.Map.fromList $ zip charlist shiftedlist

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- Error handling for reading an integer
readInt :: String -> Either String Int
readInt s =
    case reads s of
        [(n, "")] -> Right n
        _         -> Left "I cannot do that"

-- Process the encode or decode command based on the shift value
processCommand :: String -> [String] -> (Int -> String -> String) -> IO String
processCommand arg2 rest fn = 
    case readInt arg2 of
        Left err -> do
            putStrLn err
            return "Try again"
        Right arg2int -> 
            if null rest then do
                putStrLn "I cannot do that"
                return "Try again."
            else do
                putStrLn $ unwords $ map (fn arg2int) rest
                return "Done."


eventLoop :: String -> IO String
eventLoop state = 
    do 
        line <- getLine
        putStrLn $ "> " ++ line
        let args = words line
        case args of 
            ["quit"] ->  return "Bye"
            ("encode":arg2:rest) -> do 
                processCommand arg2 rest encode
                eventLoop state
            ("decode":arg2:rest) -> do 
                processCommand arg2 rest decode
                eventLoop state
            [] -> do
                putStrLn "I cannot do that"
                eventLoop state
            _ -> do 
                putStrLn "I cannot do that"
                eventLoop state

main = do
    finalState <- eventLoop initialstate
    putStrLn finalState