import Data.Time.Calendar (Day, fromGregorianValid, fromGregorian, diffDays)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.List (sort, sortBy, sortOn)
import Data.Ord (comparing)
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
instance Ord Event where
    -- Compare events only by their name
    compare event1 event2 = compare (name event1) (name event2)

type EventCalendar = Map.Map Day [Event]

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
            let existingEvents = Map.findWithDefault [] date calendar
            if any (any (\event -> name event == eventName)) (Map.elems calendar)
                then do
                    putStrLn "Event already exists"
                    return calendar
                else do
                    putStrLn "OK"
                    let event = Event {name = eventName, place = placeName}
                    let updatedCalendar = Map.insertWith (++) date [event] calendar
                    return updatedCalendar
        Nothing   -> do
            putStrLn "Bad date"
            return calendar

processNear :: String -> EventCalendar -> IO String
processNear dateString calendar = 
    case stringToDate dateString of
        Just date -> do
            -- Get all dates and filter those within 7 days
            let nearDates = sort $ Map.keys $ Map.filterWithKey (\d _ -> abs (diffDays d date) <= 7) calendar
            -- Collect nearby events
            let nearbyEvents = concatMap (\d -> map (\e -> (d, e)) (Map.findWithDefault [] d calendar)) nearDates
            if null nearbyEvents
                then do
                    putStrLn "Nothing that I know of"
                    return "No nearby events"
                else do
                    mapM_ printLine  nearbyEvents
                    return $ "Events found"
                where
                    printLine :: (Day, Event) -> IO ()
                    printLine (d, event) = 
                        putStrLn $ "Event [ " ++ name event ++ " ] happens on " ++ show d
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

processAt :: String -> EventCalendar -> IO String
processAt placeName calendar = do
    let filteredEvents = concatMap (\(date, events) -> 
                                    filter (\event -> place event == placeName) events) 
                                    (Map.toList calendar)
    if null filteredEvents
        then do
            putStrLn "Nothing that I know of"
            return "No events at place"
        else do
            mapM_ printLine filteredEvents
            return $ "Events found"
        where
            printLine :: Event -> IO ()
            printLine e = 
                putStrLn $ "Event [ " ++ name e ++ " ] happens at [ " ++ place e ++ " ]"



eventLoop :: EventCalendar -> IO String
eventLoop state = 
    do 
        --putStrLn $ "State at the moment: " ++ show state
        line <- getLine
        putStrLn $ "> " ++ line
        let args = words line
        --putStrLn $ "ARGS: " ++ show args
        case args of 
            ["Quit"] ->  return "Bye"

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

            ("Tell": "me": "about": "[": rest) -> do
                let (eventWords, afterEvent) = span (/= "]") rest
                    event = unwords eventWords
                result <- processTell event state
                eventLoop state

            ["What", "happens", "near", dateString] -> do 
                result <- processNear dateString state
                eventLoop state
            
            ("What": "happens": "at": "[": rest) -> do
                let (placeWords, afterPlace) = span (/= "]") rest
                    place = unwords placeWords
                result <- processAt place state
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

-- Inital calendar for testing locally
initCalendar :: EventCalendar
initCalendar = Map.fromList
    [ (fromGregorian 2001 02 02, [Event {name = "Event A", place = "Place A1"}])
    , (fromGregorian 2008 02 02, [Event {name = "Event G21", place = "Place G"},
                                  Event {name = "Event G1", place = "Place G"}])
    , (fromGregorian 2008 02 03, [Event {name = "Event G22", place = "Place G"}])
    , (fromGregorian 2011 02 02, [Event {name = "Event G5", place = "Place G"}])
    ]

main = do 
    --putStrLn "Welcome to calendar application"
    finalState <- eventLoop initCalendar    -- for own testing
    --finalState <- eventLoop Map.empty     -- For grader
    putStrLn finalState

-- For testing 
-- Event [ Event A ] happens at [ Place A1 ] on 2001-02-02
-- Event [ Event G21 ] happens at [ Place G ] on 2008-02-02
-- Event [ Event G1 ] happens at [ Place G ] on 2008-02-02
-- What happens near 2001-02-02
-- What happens near 2001-02-05
-- Tell me about [ Event A ]
-- Tell me about [ Event B ]
-- What happens at [ Place A1 ]