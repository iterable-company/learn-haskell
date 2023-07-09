import Data.Kind (Type)

class Eq1 (a :: Type) where
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    x === y = not (x /== y)
    x /== y = not (x === y)

data TrafficLight = Red | Yellow | Green

instance Eq1 TrafficLight where
    Red === Red = True
    Green === Green = True
    Yellow === Yellow = True
    _ === _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green light"
    show yellow = "Yellow light"
