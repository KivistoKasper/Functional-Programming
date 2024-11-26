data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other
    deriving(Show, Eq) 

data CountryCode = CountryCode Integer deriving(Show, Eq) 
toCountryCode :: Integer -> CountryCode
toCountryCode code  
    | code < 0 = error "Negative country code"
    | otherwise = CountryCode code

data PhoneNo = PhoneNo Integer deriving(Show, Eq)
toPhoneNo :: Integer -> PhoneNo
toPhoneNo num  
    | num < 0 = error "Negative phone number"
    | otherwise = PhoneNo num

data Phone = Phone
    { phoneType   :: PhoneType
    , countryCode :: CountryCode
    , phoneNo     :: PhoneNo
    } deriving (Show, Eq)



