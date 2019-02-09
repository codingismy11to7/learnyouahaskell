module Lib
  ( (|>)
  , blah
  , doubleMe
  , doubleSmallNumber
  , sortedGoodRatiosFromMax
  , ratiosToStrs
  , PiRatio()
  ) where

import           Data.List   (foldl', nub, sortOn)
import           Data.Ord    (Down (..))
import           Text.Printf (printf)

(|>) :: (Functor f) => f a -> (a -> b) -> f b
m |> f = fmap f m

places :: Int -> Int
places num
  | div10 == 0 = 1
  | otherwise = 1 + places div10
  where
    div10 = num `div` 10

someFunc :: IO ()
someFunc = putStrLn "someFunc!"

blah :: IO ()
blah = putStrLn "hi"

doubleMe x = x * 2

doubleSmallNumber :: (Ord x, Num x) => x -> x
doubleSmallNumber x =
  if x < 100
    then doubleMe x
    else x

lostNumbers :: [Integer]
lostNumbers = [1, 2, 3]

letters = ['A' .. 'Z'] ++ ['a' .. 'z']

rev :: String -> String
rev = reverse

piDelta :: PiRatio -> Double
piDelta pr = abs (pi - piRatioValue pr)

ratioCloseToPi :: PiRatio -> Bool
ratioCloseToPi pr = piDelta pr < 0.005

sortedGoodRatiosFromMax :: Int -> [PiRatio]
sortedGoodRatiosFromMax max = sortOn Data.Ord.Down goodRatios
  where
    goodRatios = getGood [1 .. max]
    getGood = filter ratioCloseToPi . nub . map piRatioFromDenom

piRatioValue :: PiRatio -> Double
piRatioValue (PiRatio n d) = fromIntegral n / fromIntegral d

piRatioFromDenom :: Int -> PiRatio
piRatioFromDenom denom =
  let numerator = round (fromIntegral denom * pi)
      gcd' = gcd numerator denom
   in PiRatio (numerator `div` gcd') (denom `div` gcd')

data PiRatio = PiRatio
  { numerator   :: Int
  , denominator :: Int
  } deriving (Eq)

instance Ord PiRatio where
  x@(PiRatio _ denomX) `compare` y@(PiRatio _ denomY) =
    case piDelta x `compare` piDelta y of
      EQ -> denomX `compare` denomY
      o  -> o

deltaString ratio = printf "%.2e" deltaRnd
  where
    deltaRnd = roundPlaces 5 $ piDelta ratio
    roundPlaces :: Int -> Double -> Double
    roundPlaces places f = fromIntegral (round (f * (10 ^ places))) / (10 ^ places)

showRatio numPlaces rat = numStr rat ++ " / " ++ denStr rat ++ "  (Δ " ++ deltaString rat ++ ")"
  where
    fmtStr places = "%" ++ show places ++ "d"
    fmt field rat = printf (fmtStr $ numPlaces field) $ field rat
    numStr = fmt numerator
    denStr = fmt denominator

ratiosToStrs ratios =
  let numPlaces field = maximum $ map (places . field) ratios
   in map (showRatio numPlaces) ratios

factorial :: (Integral x) => x -> x
factorial 0 = 1
factorial n = n * factorial (n - 1)

data Weight
  = Pounds Double
  | Kilos Double

instance Show Weight where
  show (Pounds lbs) = show lbs ++ "lbs"
  show (Kilos ks)   = show ks ++ "kgs"

toKilos :: Weight -> Double
toKilos (Kilos ks)   = ks
toKilos (Pounds lbs) = lbs / 2.205

data Height
  = Meters Double
  | FeetInches Int
               Double

instance Show Height where
  show (Meters ms)      = show ms ++ "m"
  show (FeetInches f i) = show f ++ "' " ++ show i ++ "\""

meters (Meters ms) = ms
meters (FeetInches ft inch) =
  let inches = fromIntegral ft * 12 + inch
   in inches * 2.54 / 100

bmiTell :: Height -> Weight -> String
bmiTell height weight
  | bmi <= 18.5 = "skinny"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "overweight"
  | otherwise = "obese"
  where
    wt = toKilos weight
    ht = meters height
    bmi = wt / ht ^ 2

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

describeList :: [x] -> String
describeList xs =
  "The list is " ++
  case xs of
    []     -> "empty"
    [_]    -> "a singleton"
    [_, _] -> "containing 2 items"
    _      -> "at least 3 items long"

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

max' :: (Ord a) => [a] -> a
max' []     = error "max' on empty list"
max' [x]    = x
max' (x:xs) = max x (max' xs)

replicate' :: Int -> x -> [x]
replicate' 0 elem = []
replicate' n elem = elem : replicate (n - 1) elem

repl' :: Int -> x -> [x]
repl' count elem
  | count <= 0 = []
  | otherwise = elem : repl' (count - 1) elem

take' :: Int -> [x] -> [x]
take' n _
  | n <= 0 = []
take' _ [] = []
take' count (x:xs) = x : take' (count - 1) xs

take2' :: Int -> [x] -> [x]
take2' count xs
  | count <= 0 || null xs = []
  | otherwise = head xs : take2' (count - 1) (tail xs)

rev' :: [x] -> [x]
rev' []     = []
rev' (x:xs) = rev' xs ++ [x]

{-
effreverse :: [x] -> [x]
effreverse [] = []
effreverse xs =
  let loop [] acc     = acc
      loop (x:xs) acc = loop xs (x : acc)
   in loop xs []
-}
effreverse2 :: [x] -> [x]
effreverse2 = foldl' (flip (:)) []

zip' :: [x] -> [y] -> [(x, y)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

{-
zipWithIndex :: [x] -> [(x, Int)]
zipWithIndex [] = []
zipWithIndex xs =
  let loop _ [] acc       = acc
      loop idx (x:xs) acc = loop (idx + 1) xs ((x, idx) : acc)
   in reverse (loop 0 xs [])
-}
zipWithIndex :: [x] -> [(x, Int)]
zipWithIndex xs = zipWith' (,) xs [0 ..]

elem' :: (Eq x) => x -> [x] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs

qs :: (Ord x) => [x] -> [x]
qs [] = []
qs (x:xs) =
  let smaller = qs $ filter (<= x) xs
      larger = qs $ filter (> x) xs
   in smaller ++ (x : larger)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
{-
zipWith' f = doit
  where
    doit [] _          = []
    doit _ []          = []
    doit (x:xs) (y:ys) = (f x y) : (doit xs ys)
-}
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

takeWhile' :: (x -> Bool) -> [x] -> [x]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

collatz :: Int -> [Int]
collatz 1 = [1]
collatz x =
  let newNum
        | odd x = x * 3 + 1
        | otherwise = x `div` 2
   in x : collatz newNum

numUniq :: (Ord x) => [x] -> Int
numUniq = length . nub

fox = "the quick brown fox jumps over the lazy dog"
