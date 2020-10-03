-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.

--{-# OPTIONS_GHC -F -pgmF hlint-test #-}

import Test.Tasty

import TLRS
import TFacet

main :: IO ()
main = do
  defaultMain allTests


allTests ::   TestTree
allTests = testGroup "Tasty tests" [
        testGroup "List of tests:" [
            testsFacet,
            testsLRS
            ]
    ]
