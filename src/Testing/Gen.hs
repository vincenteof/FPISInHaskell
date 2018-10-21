module Testing.Gen where

import Control.Monad.State
import Data.List
import Data.Maybe
import System.Random

type Gen a = State StdGen a

choose :: Int -> Int -> Gen Int
choose start stopExclusive = state $ randomR (start, stopExclusive)

unit :: a -> Gen a
unit = return

boolean :: Gen Bool
boolean = state random

double :: Gen Double
double = state random

listOfN :: Int -> Gen a -> Gen [a]
listOfN n gen = sequenceA (replicate n gen)

flatMap :: Gen a -> (a -> Gen b) -> Gen b
flatMap = (>>=)

listOfNGen :: Gen Int -> Gen a -> Gen [a]
listOfNGen sizeGen elemGen = do
  size <- sizeGen
  elem <- elemGen
  return (replicate size elem)

union :: Gen a -> Gen a -> Gen a
union g1 g2 =
  boolean `flatMap`
  (\b ->
     if b
       then g1
       else g2)

weighted :: (Gen a, Double) -> (Gen a, Double) -> Gen a
weighted (g1, w1) (g2, w2) = do
  d <- double
  let total = w1 + w2
      g1Ratio = w1 / total
  if g1Ratio > d
    then g1
    else g2

type FailedCase = String

type SuccessCount = Int

type TestCases = Int

data Result
  = Passed
  | Falsified FailedCase
              SuccessCount
  deriving (Show)

isFulsified :: Result -> Bool
isFulsified Passed = False
isFulsified (Falsified _ _) = True

type MaxSize = Int

newtype Prop = Prop
  { run :: (MaxSize, TestCases, StdGen) -> Result
  }

forAll :: (Show a) => Gen a -> (a -> Bool) -> Prop
forAll g f =
  Prop
    (\(max, n, rng) ->
       let rs = randomStream g rng
           zipS = take n . zip rs $ [0 ..]
           mapped =
             map
               (\(r, i) ->
                  if f r
                    then Passed
                    else Falsified (show r) i)
               zipS
           failCase = find isFulsified mapped
        in fromMaybe Passed failCase)

randomStream :: Gen a -> StdGen -> [a]
randomStream g = unfoldr $ Just . runState g

(<&&>) :: Prop -> Prop -> Prop
p1 <&&> p2 =
  Prop
    (\(max, casesNum, rng) ->
       case run p1 (max, casesNum, rng) of
         Passed -> run p2 (max, casesNum, rng)
         failCase -> failCase)

(<||>) :: Prop -> Prop -> Prop
p1 <||> p2 =
  Prop
    (\(max, casesNum, rng) ->
       case run p1 (max, casesNum, rng) of
         Passed -> Passed
         _ -> run p2 (max, casesNum, rng))

type SGen a = Int -> Gen a

unsized :: Gen a -> SGen a
unsized g _ = g

listOfSGen :: Gen a -> SGen [a]
listOfSGen g n = listOfN n g

forAllGrowing :: (Show a) => SGen a -> (a -> Bool) -> Prop
forAllGrowing sg f =
  Prop
    (\(max, n, rng) ->
       let casesPerSize = (n + (max - 1)) `div` max
           smallNum = min max n
           props = map (\i -> forAll (sg i) f) . take (smallNum + 1) $ [0 ..]
           mapF p = Prop (\(max, _, rng) -> run p (max, casesPerSize, rng))
           prop = foldl1 (<&&>) . map mapF $ props
        in run prop (max, n, rng))

totalRun :: Prop -> Int -> Int -> IO ()
totalRun prop maxSize testCases = do
  rng <- getStdGen
  case run prop (maxSize, testCases, rng) of
    Falsified msg n -> do
      putStrLn $ "! Falsified after " ++ show n ++ " passed tests:"
      putStrLn msg
    Passed -> putStrLn $ "+ Ok, passed " ++ show testCases ++ " tests."
