{-# LANGUAGE AllowAmbiguousTypes, DataKinds #-}



module TLRS (testsLRS) where

import Geometry.LRS
import Util
import TestCases

import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Data.Maybe

import Data.Matrix
   
testLRSFacetEnumeration :: TestTree
testLRSFacetEnumeration = HU.testCase "Test for facet enumeration using LRS" $ do
        --sortOutput (lrsFacetEnumeration triangle)    @?= sort (multiplyBy (-1/3)  resTr)
        sortOutput (lrsFacetEnumeration _3Cube  )    @?= sort (multiplyBy (-1)    res3C)
        sortOutput (lrsFacetEnumeration _4Cube  )    @?= sort (multiplyBy (-1/2)    res4C)
        --sortOutput (lrsFacetEnumeration _crossPolytope)   @?= sort (multiplyBy (-1/2)  resCross)
  

testsLRS :: TestTree
testsLRS = testGroup "Test for LRS version" [testLRSFacetEnumeration] 

