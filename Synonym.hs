import qualified Data.Map as Map
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name phoneNumber phoneBook = (name, phoneNumber) `elem` phoneBook

type IntMap1 v = Map.Map Int v
type IntMap2 = Map.Map Int

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just(state, code) ->
        if state /= Taken
            then Right code
            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
lockers :: LockerMap
lockers = Map.fromList [
    (100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I")) 
    ,(103,(Free, "IQSA9")) 
    ,(105,(Free, "QOTSA")) 
    ,(109,(Taken, "893JJ")) 
    ,(110,(Taken, "99292"))
    ]

