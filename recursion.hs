maxim :: Ord a => [a] -> a
maxim lst =
    let x:xs = lst
    in partial x xs
    where
        partial acc [] = acc
        partial acc (x:xs)
            | acc > x = partial acc xs
            | otherwise = partial x xs

maximu :: Ord a => [a] -> a
maximu [] = error "maximu of empty list!"
maximu [x] = x
maximu (x:xs) = max x (maximu xs)

reple :: Int -> a -> [a]
reple 0 _ = []
reple i x = x:reple (i-1) x

tak :: Int -> [a] -> [a]
tak n l
    | n < 0 = error "minus is specified!"
    | length l < n = error "too large!"
    | n == 0 = []
tak n (x:xs) = x:tak (n-1) xs

ta :: Int -> [a] -> [a]
ta n _
    | n <= 0 = []
ta _ [] = []
ta n (x:xs) = x : ta (n-1) xs

revers :: [a] -> [a]
revers [] = []
revers (x:xs) = revers xs ++ [x]

zi :: [a] -> [b] -> [(a,b)]
zi _ [] = []
zi [] _ = []
zi (x:xs) (y:ys) = (x,y):zi xs ys

repea :: a -> [a]
repea x = x:repea x

el :: Eq a => a -> [a] -> Bool
el _ [] = False
el e (x:xs) = e == x || el e xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x: filter' p xs
    | otherwise = filter' p xs

chain' :: Integer -> [Integer]
chain' 1 = [1]
chain' n
    | even n = n : chain' (n `div` 2)
    | odd n = n : chain' (n * 3 + 1)
