import Criterion.Main

import Geometry.Facet
import Geometry.LRS
import TestCases

main :: IO ()
main = defaultMain [
    bgroup "Triangle" [
        bench "Yang" $ whnf facetEnumeration     triangle,
        bench "LRS"  $ whnf lrsFacetEnumeration  triangle],

    bgroup "3-Cube" [
        bench "Yang" $ whnf facetEnumeration     _3Cube,   
        bench "LRS"  $ whnf lrsFacetEnumeration  _3Cube],

    bgroup "4-Cube" [
        bench "Yang" $ whnf facetEnumeration     _4Cube,   
        bench "LRS"  $ whnf lrsFacetEnumeration  _4Cube],

    bgroup "4-Cube" [
        bench "Yang" $ whnf facetEnumeration     _4Cube,   
        bench "LRS"  $ whnf lrsFacetEnumeration  _4Cube]
    ] 