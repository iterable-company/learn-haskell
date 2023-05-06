myQuick :: Ord a => [a] -> [a]
myQuick [] = []
myQuick (x:xs) = small x (myQuick xs) ++ [x] ++ large x (myQuick xs)
    where
        small :: Ord a => a -> [a] -> [a]
        small _ [] = []
        small p (x:xs)
            | p < x = small p xs 
            | otherwise = x: small p xs
        large :: Ord a => a -> [a] -> [a]
        large _ [] = []
        large p (x:xs)
            | p > x = large p xs 
            | otherwise = x: large p xs

quick :: (Ord a) => [a] -> [a]
quick [] = []
quick (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <=x]
        larger = [a | a <- xs, a > x]
    in quick smallerOrEqual ++ [x] ++ quick larger