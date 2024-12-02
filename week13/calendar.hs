import Data.Time.Calendar (Day, fromGregorianValid)
import Text.Read (readMaybe)
import qualified Data.Map as Map
{-
data Name = Name String deriving (Eq)
instance Show Name where 
    show (Name name) = show name

data Place = Place String deriving (Eq)
instance Show Place where 
    show (Place place) = show place
-}
data Event = Event {
    name :: String,
    place :: String
    } deriving (Eq, Show)

type EventCalendar = Map.Map Day [Event]

-- Error handling for reading an integer
readInt :: String -> Either String Int
readInt s =
    case reads s of
        [(n, "")] -> Right n
        _         -> Left "String Int error"
-- Convert String to Day
stringToDate :: String -> Maybe Day
stringToDate dateString = do
    let args = words (map (\c -> if c == '-' then ' ' else c) dateString) -- Replace '-' with spaces
    case args of 
        [year, month, day] -> do
            let y = read year :: Integer
            let m = read month :: Int
            let d = read day :: Int
            fromGregorianValid y m d -- Validate and return a valid date
        _ -> Nothing -- Return Nothing for an invalid format


processEvent :: String -> String -> String -> EventCalendar -> IO EventCalendar
processEvent eventName placeName dateString calendar = do
    case stringToDate dateString of
        Just date -> do
            putStrLn "OK"
            -- Create an Event and insert it into the calendar
            let event = Event {name = eventName, place = placeName} -- Add a placeholder for place
            let updatedCalendar = Map.insertWith (++) date [event] calendar
            return updatedCalendar
        Nothing   -> do
            putStrLn "Bad date"
            return calendar

processNear :: String -> EventCalendar -> IO String
processNear dateString calendar = 
    case stringToDate dateString of
        Just date -> 
            case Map.lookup date calendar of
                Just lst -> do
                    putStrLn $ unlines (map show lst)
                    return "OK"
                Nothing -> do
                    putStrLn "Not found"
                    return "NOT OK"
        Nothing -> do
            putStrLn "Bad date"
            return "NOT OK"

processTell :: String -> EventCalendar -> IO String
processTell eventName calendar = do
    let matchingDays = Map.toList $ Map.filter (any (\event -> name event == eventName)) calendar
    --putStrLn $ "found: " ++ show matchingDays
    case matchingDays of
        ((day, events):_) -> do
            -- Find the first matching event
            let event = head events  -- Assuming at least one event is found
            putStrLn $ "Event [ " ++ name event ++ " ] happens at [ " ++ place event ++ " ] on " ++ show day
            return "found"
        [] -> do
            putStrLn "I do not know of such event"
            return "not found"

eventLoop :: EventCalendar -> IO String
eventLoop state = 
    do 
        --putStrLn $ "State at the moment: " ++ show state
        line <- getLine
        putStrLn $ "> " ++ line
        let args = words line
        --putStrLn $ "ARGS: " ++ show args
        case args of 
            ["quit"] ->  return "Bye"
            ("Event" : "[" : rest) -> do 
                let (eventWords, afterEvent) = span (/= "]") rest
                    (_:afterBracket) = afterEvent  -- Skip the closing "]"
                    ("happens":"at":"[":placeRest) = afterBracket
                    (placeWords, afterPlace) = span (/= "]") placeRest
                    (_:"on":dateString:[]) = afterPlace
                    event = unwords eventWords
                    place = unwords placeWords
                --putStrLn $ event ++ " " ++ place ++ " " ++ dateString
                result <- processEvent event place dateString state
                eventLoop result
                --eventLoop state
            ["What", "happens", "near", dateString] -> do 
                result <- processNear dateString state
                eventLoop state
            ("Tell": "me": "about": "[": rest) -> do
                let (eventWords, afterEvent) = span (/= "]") rest
                    event = unwords eventWords
                --putStrLn $ event
                processTell event state
                eventLoop state
            ("test":"[":event:"]": []) -> do
                putStrLn $ "test event: " ++ event
                eventLoop state
            [] -> do
                putStrLn "I do not understand that"
                eventLoop state
            _ -> do 
                putStrLn "I do not understand that"
                eventLoop state


main = do 
    putStrLn "Welcome to calendar application"
    --finalState <- eventLoop phoneBook -- for own testing
    finalState <- eventLoop Map.empty -- For grader
    putStrLn finalState

-- For testing 
-- Event [ Event A ] happens at [ Place A1 ] on 2001-02-02
-- Event [ Event G21 ] happens at [ Place G ] on 2008-02-02
-- Event [ Event G1 ] happens at [ Place G ] on 2008-02-02
-- What happens near 2001-02-02
-- What happens near 2008-02-02
-- Tell me about [ Event A ]
-- Tell me about [ Event B ]