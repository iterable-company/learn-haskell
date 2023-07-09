{-# LANGUAGE DuplicateRecordFields #-}
data Person = Person {
    firstName :: String
    , lastName :: String
    , age :: Int
    , height :: Float
    , phoneNumber :: String
    , flavor :: String
} deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n ) = Vector (i+l) (j+m) (k+n) 

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

data Person1 = Person1 {
    firstName :: String,
    lastName :: String,
    age :: Int
} deriving (Eq, Show, Read)
makePerson1 :: String -> String -> Int -> Person1
makePerson1 f l a = Person1 { firstName = f, lastName = l, age = a }

