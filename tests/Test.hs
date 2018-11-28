{-# language ScopedTypeVariables #-}

module Main where

import System.IO
import Control.Exception
import Test.Tasty
import Test.Tasty.Golden
import Control.Monad

import ConSubSeq

test :: Bool -> String -> FilePath -> FilePath -> TestTree
test valid testName fIn fOut =
  -- XXX: Avoid name clashes in the '/tmp' directory.
  let fTmp = "/tmp/test-" ++ testName in
  goldenVsFile testName fOut fTmp $ do
    withFile fIn ReadMode $ \hIn ->
      withFile fTmp WriteMode $ \hTmp ->
        catch (run hIn hTmp hTmp) $ \(e :: SomeException) ->
          when valid $ throwIO e

testData :: Bool -> String -> TestTree
testData valid name =
  test valid name
    ("./data/" ++ name ++ ".in")
    ("./data/" ++ name ++ ".out")

main :: IO ()
main = defaultMain $ testGroup "Tests" $
  [ testData True "1-eq"
  , testData True "1-neq"
  , testData False "1-inv-comma"
  , testData False "1-inv-range"

  , testData True "2-group"
  , testData True "2-no-group"

  , testData True "3-group"
  , testData True "3-no-group"

  , testData True "sample"
  ]
