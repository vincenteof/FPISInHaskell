module Main where

import Testing.Gen

main :: IO ()
main =
  let smallInt = choose (-10) 10
      maxProp =
        forAllGrowing
          (listOfSGen smallInt)
          (\ns ->
             let maxElem = maximum ns
              in all (<= maxElem) ns)
   in totalRun maxProp 100 100
