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
stringToDate :: String -> String -> String -> Maybe Day
stringToDate year month day = do
    let y = read year :: Integer
    let m = read month :: Int
    let d = read day :: Int
    fromGregorianValid y m d

processEvent :: String -> String -> String -> String -> EventCalendar -> IO EventCalendar
processEvent eventName year month day calendar = do
    case stringToDate year month day of
        Just date -> do
            putStrLn "OK"
            -- Create an Event and insert it into the calendar
            let event = Event {name = eventName, place = ""} -- Add a placeholder for place
            let updatedCalendar = Map.insertWith (++) date [event] calendar
            return updatedCalendar
        Nothing   -> do
            putStrLn "NOT OK"
            return calendar

processAt :: String -> String -> String -> EventCalendar -> IO String
processAt year month day calendar = 
    case stringToDate year month day of
        Just date -> 
            case Map.lookup date calendar of
                Just lst -> do
                    putStrLn $ unlines (map show lst)
                    return "OK"
                Nothing -> do
                    putStrLn "Not found"
                    return "NOT OK"
        Nothing -> do
            putStrLn "Invalid date"
            return "NOT OK"

eventLoop :: EventCalendar -> IO String
eventLoop state = 
    do 
        --putStrLn $ "State at the moment: " ++ show state
        line <- getLine
        putStrLn $ "> " ++ line
        let args = words line
        case args of 
            ["quit"] ->  return "Bye"
            ["Event",event,year,month,day] -> do 
                result <- processEvent event year month day state
                
                --eventLoop result
                eventLoop result
            ["What", "happens", "at", year, month, day] -> do 
                result <- processAt year month day state
                eventLoop state
            {-
            ["What", "happens", "near", date] -> do 
                result <- processWhat date
                eventLoop state
            -}
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