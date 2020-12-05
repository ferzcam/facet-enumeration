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
        
    bgroup "Octahedron" [
        bench "Yang" $ whnf facetEnumeration     octahedron,   
        bench "LRS"  $ whnf lrsFacetEnumeration  octahedron],
    
    bgroup "Cross-Polytope" [
        bench "Yang" $ whnf facetEnumeration     _crossPolytope,   
        bench "LRS"  $ whnf lrsFacetEnumeration  _crossPolytope],

    bgroup "Pyramid" [
        bench "Yang" $ whnf facetEnumeration     lrsPaper,   
        bench "LRS"  $ whnf lrsFacetEnumeration  lrsPaper]
    ] 
