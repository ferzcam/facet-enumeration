module TestCases where

import Data.Matrix
import Data.Function
import Data.List hiding (transpose)

-- To modify output of Yang's paper
multiplyBy :: Rational -> [([Rational], Rational)] -> [([Rational], Rational)]
multiplyBy factor list = map (\(hyp, c) -> (map (*factor) hyp, factor * c)) list

-- To modify output of LRS
sortOutput :: (Matrix Rational, Matrix Rational) -> [([Rational], Rational)]
sortOutput (mat, vec) = sort $ zip matLists vecList
    where
        matLists = toLists mat
        vecList  = head $ toLists $ transpose vec

-------------------------------------
triangle    = [[0,0],[3,0],[0,3]]
hyperTr     = [[0,-1], [-1,0], [1,1]]   :: [[Rational]]
bTr         = [0,0,3]                   :: [Rational]
resTr       = zip hyperTr bTr
-------------------------------------
_3Cube      = [[0,0,0],[0,0,1],[1,1,1],[0,1,1],[1,0,0],[1,0,1],[1,1,0],[0,1,0]]
hyper3C     = [
                [-1, 0, 0],
                [ 0,-1, 0],
                [ 0, 0,-1],
                [ 0, 0, 1],
                [ 0, 1, 0],
                [ 1, 0, 0]
            ] :: [[Rational]]
b3C         = [0, 0, 0, 1, 1, 1] :: [Rational]
res3C       = zip hyper3C b3C
-------------------------------------


_4Cube = [
    [0,0,0,0],
    [0,0,0,1],
    [0,0,1,0],
    [0,0,1,1],
    [1,1,1,1],
    [0,1,0,1],
    [1,1,0,0],
    [0,1,1,0],
    [0,1,1,1],
    [1,0,0,0],
    [1,0,0,1],
    [1,0,1,0],
    [1,0,1,1],
    [1,1,0,1],
    [1,1,1,0],
    [0,1,0,0]]

hyper4C = [
    [-2, 0, 0, 0],
    [ 0,-2, 0, 0],
    [ 0, 0,-2, 0],
    [ 0, 0, 0,-2],
    [ 0, 0, 0, 2],
    [ 0, 0, 2, 0],
    [ 0, 2, 0, 0],
    [ 2, 0, 0, 0]
    ] :: [[Rational]]

b4C = [
        0,
        0,
        0,
        0,
        2,
        2,
        2,
        2
    ] :: [Rational]

res4C = zip hyper4C b4C

-----------------------------

lrsPaper = [
    [ 1, 1, 0],
    [-1, 1, 0],
    [ 1,-1, 0],
    [-1,-1, 0],
    [ 0, 0,-1]
    ]

hyperLrs = [
    [-1, 0, 1],
    [ 0,-1, 1],
    [ 0, 0,-2],
    [ 0, 1, 1],
    [ 1, 0, 1]] :: [[Rational]]

bLrs = [
    -1,
    -1,
     0,
    -1,
    -1
    ] :: [Rational]

resLrs = zip hyperLrs bLrs