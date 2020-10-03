{-# LANGUAGE AllowAmbiguousTypes, DataKinds #-}



module TLRS (testsLRS) where

import Geometry.LRS
import Util

import Test.Tasty
import Test.Tasty.HUnit as HU
import Data.List
import Data.Maybe

import Data.Matrix
   

mat = fromLists [
                        [-1,0,0],
                        [0,-1,0],
                        [0,0,-1],
                        [1,0,0],
                        [0,1,0],
                        [0,1,1],
                        [0,-1,1],
                        [1,0,1],
                        [-1,0,1]
                    ]

b = colFromList [0,0,0,1,1,2,1,2,1]

mat1 = (fromLists [[1,0,-2],[1,-1,0],[0,-1,0],[-1,0,-1]])



vertices1 = [[0,0],[3,0],[0,3]]
facets1 = [[1,3],[1,2],[2,3]]
hyperplanes1 = [[0,-1], [-1,0], [1,1]]
b1 = [0,0,3]
res1 = safeZipWith3 (,,) facets1 hyperplanes1 b1

vertices2 = [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]
facets2 = [[1,3,4,2],[1,5,6,2],[1,5,7,3],[2,6,8,4],[3,7,8,4],[5,7,8,6]]

hyperplanes2 = [[-2,0,0], [0,-2,0], [0,0,-2], [0,0,2], [0,2,0], [2,0,0]]
b2 = [0,0,0,2,2,2]
res2 = safeZipWith3 (,,) facets2 hyperplanes2 b2



testVertexEnum :: TestTree
testVertexEnum = HU.testCase "Test for LRS in two variable polys" $ do
        lrs mat1 (colFromList [0,0,0,0]) [0,0,0] @?= [[(-2)/3,(-2)/3,(-1)/3],[0,(-1),0],[0,0,(-1)],[1,0,(-1)]]

testLRSFacetEnumeration :: TestTree
testLRSFacetEnumeration = HU.testCase "Test for facet enumeration using LRS" $ do
        lrsFacetEnumeration vertices1 @?= (fromLists hyperplanes1, colFromList b1)
        lrsFacetEnumeration vertices2 @?= (fromLists hyperplanes2, colFromList b2)


testsLRS :: TestTree
testsLRS = testGroup "Test for LRS version" [testLRSFacetEnumeration] 

