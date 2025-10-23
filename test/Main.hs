module Main (main) where

import Control.Recursive.Subset
import Test.QuickCheck

subset :: [Int] -> Int -> Bool
subset xs t = s xs t == rs' xs t

propSubset :: Property
propSubset = 
    forAll genXs $ \xs -> 
    forAll genT  $ \t  -> subset xs t
    where genXs = choose (0, 25) >>= \n ->
                  vectorOf n (choose (0,1000))
          genT  = choose (0,1250)

propTinySubset :: Property
propTinySubset = 
    forAll genXs $ \xs -> 
    forAll genT  $ \t  -> subset xs t
    where genXs = choose (0, 25) >>= \n ->
                  vectorOf n (choose (0,10))
          genT  = choose (0,250)

main :: IO ()
main = quickCheck propSubset >>
       quickCheck propTinySubset