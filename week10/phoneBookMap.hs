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

type Name = String
type PhoneBook = Map.Map Name [Phone]

phoneBook :: PhoneBook
phoneBook = Map.fromList
  [
  ("Markku", [readPhone "WorkMobile" "358" "123456"]),
  ("Erkki", [readPhone "PrivateMobile" "358" "221456", 
             readPhone "WorkMobile" "358" "3333"]),
  ("jouni", [readPhone "Other" "" "78946123"])
  ]

findEntries :: Name -> PhoneBook -> [Phone]
findEntries name phonebook = case Map.lookup name phonebook of 
    Just lst -> lst
    Nothing -> []
    

-- findEntries "Erkki" phoneBook
-- findEntries "Sirpa" phoneBook

addEntry :: Name -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry name phonetype ccode phonenum currentbook = 
    let newEntry = readPhone phonetype ccode phonenum
        newLst = case Map.lookup name currentbook of
            Just lst -> 
                if any (\phone -> (phoneNo phone) == (phoneNo newEntry) ) lst then lst
                else newEntry : lst
            Nothing -> [newEntry]
    in Map.insert name newLst currentbook 
    
emptyBook :: PhoneBook
emptyBook = Map.empty

-- y = addEntry "Kalle" "Other" "48" "11111" phoneBook
-- c = addEntry "Kalle" "Other" "48" "11111" y