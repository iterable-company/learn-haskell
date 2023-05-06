import Distribution.Simple.Utils (xargs)
import Distribution.TestSuite (TestInstance(name))
doubleMe x = x + x
doubleUs x y = x * 2 + y * 2
factorial :: Integer -> Integer
factorial n = product[1..n]
intermidiatePoint :: (Double, Double) -> (Double, Double) -> (Double, Double)
intermidiatePoint x y = ((fst x  + fst y)/2.0, (snd x + snd y)/2.0)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one elment: " ++ show x 
tell (x:_:_) = "The list is long. The first elment is: " ++ show x

greet :: String -> String
greet name
    | name == "Juan" = niceGreeting ++ "Juan"
    | name == "Fernando" = niceGreeting ++ "Fernando"
    | otherwise = badGreeting ++ " " ++ name
    where
        niceGreeting = "Hello!"
        badGreeting = "Oh!"

inter :: [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)]
inter xs ys = [intermidiate x y| x <- xs, y <- ys]
    where
        intermidiate (x1, x2) (y1, y2) = ((x1 + y1)/2.0, (x2 + y2)/2.0)

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f i = f (f i)

