import qualified Data.Time.Calendar as Calendar
import Text.Read (readMaybe)

data Name = Name String deriving (Eq)
instance Show Name where 
    show = (Name name) = show name

data Place = Place String deriving (Eq)
instance Show Place where 
    show (Place place) = show place

data Event = Event {
    name :: Name,
    place :: Place,
} deriving(Eq)

type EventCalendar = Map.Map String [Event]

eventLoop :: EventCalendar -> IO String
eventLoop state = 
    do 
        line <- getLine
        putStrLn $ "> " ++ line
        let args = words line
        case args of 
            ("quit":_) ->  return "Bye"
            ("add":rest) -> do 
                result <- processAdd rest addEntry state
                putStrLn "Done" 
                eventLoop result
            ("find":name:_) -> do 
                result <- processFind name findEntries state
                --putStrLn result 
                eventLoop state
            _ -> do 
                putStrLn "I cannot do that"
                eventLoop state


main = do 
    putStrLn "Welcome to calendar application"
    --finalState <- eventLoop phoneBook -- for own testing
    finalState <- eventLoop Map.Empty -- For grader
    putStrLn finalState