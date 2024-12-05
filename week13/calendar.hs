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

processNear :: String -> EventCalendar -> IO String
processNear dateString calendar = 
    case stringToDate dateString of
        Just date -> do
            -- Collect nearby events
            let nearbyEvents = Map.toList $
                               Map.map (map name) $
                               Map.filterWithKey (\d _ -> abs (diffDays d date) <= 7) calendar
            if null nearbyEvents
                then do
                    putStrLn "Nothing that I know of"
                    return "No nearby events"
                else do
                    let sortedEvents = map (\(d, e) -> (d, sort e)) nearbyEvents
                    mapM_ printLine  sortedEvents
                    return $ "Events found"
                where
                    printLine :: (Day, [String]) -> IO ()
                    printLine (d, events) = 
                        mapM_ (\e -> putStrLn $ "Event [ " ++ e ++ " ] happens on " ++ show d) events
        Nothing -> do
            putStrLn "Bad date"
            return "NOT OK"

-- What happens at [ Place G ]
processAt :: String -> EventCalendar -> IO String
processAt placeName calendar = do
    let filteredEvents =  concatMap (\(_, events) -> map (\event -> (name event, place event)) events) $ 
                          Map.toList $ 
                          Map.map (filter (\event -> place event == placeName)) calendar                         
    if null filteredEvents
        then do
            putStrLn "Nothing that I know of"
            return "No events at place"
        else do
            let sortedEvents = sort filteredEvents 
            mapM_ printLine sortedEvents
            return $ "Events found"
        where
            printLine :: (String, String) -> IO ()
            printLine (e, p) = 
                putStrLn $ "Event [ " ++ e ++ " ] happens at [ " ++ p ++ " ]"



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
-- What happens near 2008-02-05
-- What happens near 2001-02-05
-- Tell me about [ Event A ]
-- Tell me about [ Event B ]
-- What happens at [ Place A1 ]
-- What happens at [ Place G ]