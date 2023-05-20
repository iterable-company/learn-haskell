import qualified Data.Map as Map
import Data.Char(isDigit, digitToInt)

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head $ filter (\(x, v) -> x == key) xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k, v):xs)
    | key == k = Just v
    | otherwise = findKey' key xs

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key xs = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing xs

phoneToDigit phone = map digitToInt $ filter (\ch -> isDigit ch) phone

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
    where add number1 number2 = number1 ++ ", " ++ number2