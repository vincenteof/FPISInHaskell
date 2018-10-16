module Testing.Gen
(
) where

import Control.Monad.State
import System.Random
import Data.List
import Data.Maybe

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
union g1 g2 = boolean `flatMap` (\ b -> if b then g1 else g2)

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

data Result = Passed | Falsified FailedCase SuccessCount deriving (Show)

isFulsified :: Result -> Bool
isFulsified Passed = False
isFulsified (Falsified _ _) = True 

newtype Prop = Prop { run :: (TestCases, StdGen) -> Result}

forAll :: (Show a) => Gen a -> (a -> Bool) -> Prop
forAll g f = Prop (\ (n, rng) ->
    let rs = randomStream g rng
        zipS = zip rs [0..n]
        mapped = map (\ (r, i) -> if f r then Passed else Falsified (show r) i) zipS
        failCase = find isFulsified mapped
    in fromMaybe Passed failCase)


randomStream :: Gen a -> StdGen -> [a]
randomStream g = unfoldr $ Just . runState g


(<&&>) :: Prop -> Prop -> Prop
p1 <&&> p2 = Prop (\ (casesNum, rng) -> 
    case run p1 (casesNum, rng) of 
        Passed -> run p2 (casesNum, rng) 
        failCase -> failCase)