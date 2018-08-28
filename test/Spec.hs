{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Matrix.Static
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [ docExamples ]

docExamples :: TestTree
docExamples = testGroup "Doc examples"
  [ testCase "mapRowUnsafe" $
      (mapRowUnsafe (\_ x -> x + 1) 2 $ fromListUnsafe @3 @3 @Int [1..9]) @?=
        fromListUnsafe [1,2,3,5,6,7,7,8,9]
  , testCase "mapRow" $
      (mapRow @2 (\_ x -> x + 1) $ fromListUnsafe @3 @3 @Int [1..9]) @?=
        fromListUnsafe [1,2,3,5,6,7,7,8,9]
  , testCase "mapColUnsafe" $
    (mapColUnsafe (\_ x -> x + 1) 2 $ fromListUnsafe @3 @3 @Int [1..9]) @?=
        fromListUnsafe [1,3,3,4,6,6,7,9,9]
  , testCase "mapCol" $
      (mapCol @2 (\_ x -> x + 1) $ fromListUnsafe @3 @3 @Int [1..9]) @?=
        fromListUnsafe [1,3,3,4,6,6,7,9,9]

  ]
