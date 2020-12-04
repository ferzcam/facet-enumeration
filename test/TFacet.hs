module TFacet where


import Geometry.Facet
import Geometry.Vertex
import Util 

import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Data.Matrix
import Data.List
import Data.Maybe
import TestCases




proj :: [( Facet, Hyperplane, Rational)] -> [(Hyperplane, Rational)]
proj = map (\(_,b,c) -> (b,c))

testFacetEnum :: TestTree
testFacetEnum =   HU.testCase "Tests for facet enumeration algorithm" $ do
    sort (proj $ facetEnumeration triangle) @?= sort resTr
    sort (proj $ facetEnumeration _3Cube)   @?= (sort $ multiplyBy 2 res3C)
    sort (proj $ facetEnumeration _4Cube)   @?= (sort $ res4C)


testsFacet :: TestTree
testsFacet = testGroup "Test for facets" [testFacetEnum]



