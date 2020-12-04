{-# LANGUAGE TypeSynonymInstances, FlexibleInstances#-}

module Geometry.Matrix where
    
import Numeric.LinearAlgebra.HMatrix
import qualified Data.Matrix as M
import Data.Function
-- instance Element Rational


-- getIndependent :: [[Rational]] -> [[Rational]] -> Int -> Int -> M.Matrix Rational-> [[Rational]]
-- getIndependent initial accum currRank maxRank slackMat
--     | currRank == maxRank = accum
--     | accum == [] = getIndependent (tail initial) (return $ head initial) 1 maxRank slackMat
--     | otherwise =   if newRank > currRank 
--                     then getIndependent (tail initial) (candidate:accum) (currRank+1) maxRank slackMat
--                     else getIndependent (tail initial) accum currRank maxRank slackMat
--         where
--             slack = map (map fromRational) $ M.toLists slackMat
--             candidate = head initial
--             setVectors = map (1:) $ candidate:accum
--             matrix =  fromLists $ zipRev ((map (map fromRational)) setVectors :: [[Double]]) (slack)
--             newRank = rank matrix

getIndependent :: [[Rational]] -> [[Rational]] -> Int -> Int -> M.Matrix Rational-> [[Rational]]
getIndependent initial accum currRank maxRank slackMat
    | currRank == maxRank = accum
    | accum == [] = getIndependent (tail initial) (return $ head initial) 1 maxRank slackMat
    | otherwise =   if newRank > currRank 
                    then getIndependent (tail initial) (candidate:accum) (currRank+1) maxRank slackMat
                    else getIndependent (tail initial ++ [candidate]) accum currRank maxRank slackMat
        where
            slack = map (map fromRational) $ M.toLists slackMat
            candidate = head initial
            setVectors = map (1:) $ candidate:accum
            matrix =  fromLists $ zipRev ((map (map fromRational)) setVectors :: [[Double]]) (slack)
            newRank = rank matrix

zipRev :: [[Double]] -> [[Double]] -> [[Double]]
zipRev a = (reverse . ((zipWith (++)) `on` reverse) a) 