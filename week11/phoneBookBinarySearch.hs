import Text.Read (readMaybe)
import qualified Data.Map as Map


data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other
    deriving(Show, Eq, Read) 

-- Country code
data CountryCode = CountryCode Integer deriving(Eq) 
instance Show CountryCode where
    show (CountryCode code) = show code

toCountryCode :: Integer -> CountryCode
toCountryCode code  
    | code < 0 = error "Negative country code"
    | otherwise = CountryCode code


-- Phone number
data PhoneNo = PhoneNo Integer deriving(Eq)
instance Show PhoneNo where
    show (PhoneNo num) = show num

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num  
    | num < 0 = error "Negative phone number"
    | otherwise = PhoneNo num

-- Phone 
data Phone = Phone
    { phoneType   :: Maybe PhoneType
    , countryCode :: Maybe CountryCode
    , phoneNo     :: PhoneNo
    } deriving (Eq)

instance Show Phone where
    show Phone {phoneNo=n, countryCode=c, phoneType=t} = 
      let
            countryCodeStr = case c of
                Just c -> "+" ++ show c ++ " "
                Nothing   -> ""
            phoneTypeStr = case t of
                Just t -> " (" ++ show t ++ ")"
                Nothing    -> ""
        in
            countryCodeStr ++ show n ++ phoneTypeStr

-- Reading functions
readPhoneType :: String -> Maybe PhoneType
readPhoneType "" = Nothing
readPhoneType str = case readMaybe str of 
    Just str -> Just str
    Nothing -> error "Incorrect phone type"

readCountryCode :: String -> Maybe CountryCode
readCountryCode "" = Nothing
readCountryCode str = case readMaybe str of
    Just str -> Just (toCountryCode str)
    Nothing  -> error "Incorrect country code"

readPhoneNo :: String -> PhoneNo
readPhoneNo str = case readMaybe str of
    Just num -> toPhoneNo num       
    Nothing  -> error "Incorrect phone number"

-- Function for reading phone
readPhone :: String -> String -> String -> Phone
readPhone phonetypestr countrycodestr phonenostr = 
    let rn = readPhoneNo phonenostr
        rc = readCountryCode countrycodestr
        rt = readPhoneType phonetypestr
    in Phone rt rc rn

-- Examples for testing
--show(Phone Nothing Nothing (toPhoneNo 123))
--show(Phone Nothing (Just (toCountryCode 358)) (toPhoneNo 123))
--show(Phone (Just WorkMobile) (Just (toCountryCode 358)) (toPhoneNo 123))

--readPhoneType "PrivateMobile"
--readCountryCode "358"
--readPhoneNo "123"

--readPhone "WorkMobile" "358" "123456"
--readPhone "WorkMobile" "" "123456"

-- week 10
-- old types 
--type Name = String
--type PhoneBook = Map.Map Name [Phone]

-- week 11
type Name = String
data PhoneBook = Empty | Node String [Phone] PhoneBook PhoneBook deriving (Show,Eq)

initalNums = [
  ("Markku", [readPhone "WorkMobile" "358" "123456"]),
  ("Erkki", [readPhone "PrivateMobile" "358" "221456", 
             readPhone "WorkMobile" "358" "3333"]),
  ("Arto", [readPhone "Other" "" "99999999"]),        
  ("Jouni", [readPhone "Other" "" "78946123"])
  ]

singleton :: Name -> [Phone] -> PhoneBook 
singleton x lst = Node x lst Empty Empty

addEntry :: Name -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry name phonetype ccode phonenum Empty = singleton name [readPhone phonetype ccode phonenum]
addEntry name phonetype ccode phonenum (Node n phones left right)

    | name < n  = Node n phones (addEntry name phonetype ccode phonenum left) right
    | name > n  = Node n phones left (addEntry name phonetype ccode phonenum right)
    | otherwise = let newEntry = readPhone phonetype ccode phonenum
                  in  Node n (if any (\phone -> (phoneNo phone) == (phoneNo newEntry)) phones
                           then phones  -- Don't add if the phone number already exists
                           else phones ++ [newEntry]) left right

initializePhoneBook :: [(String, [Phone])] -> PhoneBook
initializePhoneBook entries = 
    foldr insert Empty initalNums
    where 
      insert :: (String, [Phone]) -> PhoneBook -> PhoneBook
      insert (name, phones) Empty = singleton name phones
      insert(name, phones) (Node n phoneLst left right)
        | name < n  = Node n phoneLst (insert (name, phones) left) right
        | name > n  = Node n phoneLst left (insert (name, phones) right)
        | otherwise = Node n (phoneLst ++ phones) left right 

-- Inital phonebook
phoneBook :: PhoneBook
phoneBook = initializePhoneBook initalNums 

-- x = addEntry "MMM" "WorkMobile" "333" "8888888" phoneBook
-- y = addEntry "MMM" "Other" "333" "8888888" x

findEntries :: Name -> PhoneBook -> [Phone]
findEntries _ Empty = []
findEntries name (Node n phones left right)
    | name < n  = findEntries name left  -- If the name is smaller, go to the left subtree.
    | name > n  = findEntries name right -- If the name is larger, go to the right subtree.
    | otherwise = phones  -- If the names match, return the list of phones.

-- findEntries "Markku" phoneBook

emptyBook :: PhoneBook
emptyBook = Empty