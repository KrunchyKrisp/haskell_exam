module Main where
import Data.Char

main :: IO ()
main = do
        putStrLn "Hello, Haskell!"

-- 1
lmax :: [Integer] -> [Integer]
lmax [] = []
lmax [a] = []
lmax [a,b] = []
lmax (a:(b:(c:xs)))
        | b > a && b > c = b : lmax (c:xs)
        | otherwise = lmax (b:(c:xs))

-- 2
xorEven :: [Bool] -> Bool
xorEven xs = foldl (/=) True xs

-- 3
data Tree a = Leaf a | Branch (Tree a) (Tree a)

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Branch x y) = abs (count x - count y) <= 1 && balanced x && balanced y
    where
        count (Leaf _) = 1
        count (Branch a b) = count a + count b

--4
perfect :: [Integer]
perfect = [k | k <- [4..], k == sum (divs k)]
    where
        divs k = [i | i <- [1..(k-1)], mod k i == 0]

--5
data Formula a = Pred (a -> Bool) | And (Formula a) (Formula a) | Or (Formula a) (Formula a) | Neg (Formula a)

myFilter :: Formula a -> [a] -> [a]
myFilter f (a:xs)
    | apply f a = a : myFilter f xs
    | otherwise = myFilter f xs
        where
            apply (Pred f) a = f a
            apply (And f1 f2) a = apply f1 a && apply f2 a
            apply (Or f1 f2) a = apply f1 a || apply f2 a
            apply (Neg f) a = not (apply f a)

--6 no idea really
data Input
data Output

process :: (Input -> [Integer]) -> Integer -> (Integer -> Char) -> ([Char] -> Output) -> Input -> Output
process f1 bound f2 f3 = f3 . filter isLower . map f2 . takeWhile (> bound) . f1