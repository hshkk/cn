module Main (main) where

import Control.Recursive.Subset
import Test.QuickCheck

subset :: [Int] -> Int -> Bool
subset xs t = s xs t == rs' xs t

propTinySubset :: Property
propTinySubset = 
    forAll mkxs $ \xs -> 
    forAll mkt  $ \t  -> subset xs t
    where mkxs = choose (0, 25) >>= \n ->
                 vectorOf n (choose (0,10))
          mkt  = choose (0,250)

propSubset :: Property
propSubset = 
    forAll mkxs $ \xs -> 
    forAll mkt  $ \t  -> subset xs t
    where mkxs = choose (0, 25) >>= \n ->
                 vectorOf n (choose (0,1000))
          mkt  = choose (0,1250)

main :: IO ()
main = quickCheck propTinySubset >> 
       quickCheck propSubset