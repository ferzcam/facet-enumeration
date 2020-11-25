module Main where

import Geometry.Vertex
import Geometry.Facet
import Geometry.LRS
import Util

vertices1 :: [Vertex]
vertices1 = [[0,0],[3,0],[0,3]]
facets1 = [[1,3],[1,2],[2,3]]
hyperplanes1 = [[0,-1], [-1,0], [1,1]]
b1 = [0,0,3]
res1 = safeZipWith3 (,,) facets1 hyperplanes1 b1

vertices2 :: [Vertex]
vertices2 = [[0,0,0],[0,0,1],[0,1,0],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[1,1,1]]
facets2 = [[1,3,4,2],[1,5,6,2],[1,5,7,3],[2,6,8,4],[3,7,8,4],[5,7,8,6]]

hyperplanes2 = [[-2,0,0], [0,-2,0], [0,0,-2], [0,0,2], [0,2,0], [2,0,0]]
b2 = [0,0,0,2,2,2]
res2 = safeZipWith3 (,,) facets2 hyperplanes2 b2

vertices3 :: [Vertex]
vertices3 = [
    [0,0,0,0],
    [0,0,0,1],
    [0,0,1,0],
    [0,0,1,1],
    [1,1,1,1],
    [0,1,0,1],
    [0,1,1,0],
    [0,1,1,1],
    [1,0,0,0],
    [1,0,0,1],
    [1,0,1,0],
    [1,0,1,1],
    [1,1,0,0],
    [1,1,0,1],
    [1,1,1,0],
    [0,1,0,0]]


main :: IO ()
main = do
    print "Hello to the facet enumeration package"
