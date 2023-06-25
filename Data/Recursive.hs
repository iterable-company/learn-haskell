data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
infixr 5 :-:
data List2 a = Empty2 | a :-: (List2 a) deriving (Show, Read, Eq, Ord)
infixr 5 ^++
(^++) :: List2 a -> List2 a -> List2 a
Empty2 ^++ ys = ys
(x:-:xs) ^++ ys = x :-: (xs ^++ ys) -- (x:-:xs)はパターンマッチ。パターンマッチは値コンストラクタに対して適用される。8, 'a' なども値コンストラクタ