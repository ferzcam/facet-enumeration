{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Geometry.LRS (
    -- * Types
    Dictionary,
    -- * Functions
    getDictionary,
    selectPivot,
    reverseRS,
    lrs,
    polytope2LiftedCone,
    liftedCone2polyotpe,
    lrsFacetEnumeration,

    colFromList
    )   
 where

import Geometry.Vertex
import Geometry.Matrix

import Data.Matrix as M hiding (trace)
import qualified Data.Vector as V
import Data.Maybe
import Data.List
import Util
import Control.Lens 
import Debug.Trace
import Numeric.LinearProgramming hiding (simplex)
import Numeric.LinearAlgebra.HMatrix as HM
import Data.List

type Row = M.Matrix Rational
type Col = M.Matrix Rational

data Dictionary = Dict {
                            __B :: [Int], 
                            __N :: [Int], 
                            _dict :: M.Matrix Rational
                        } 
    deriving (Show, Eq)


makeLenses ''Dictionary

numRows :: Dictionary -> Int
numRows dictionary = nrows $ dictionary^.dict

numCols :: Dictionary -> Int
numCols dictionary = ncols $ dictionary^.dict

(|*|) = multStrassen


colFromList :: [a] -> M.Matrix a
colFromList = colVector . V.fromList

rowFromList :: [a] -> M.Matrix a
rowFromList = rowVector . V.fromList

-- | Wrapper of submatrix function. This function works for indices starting from 0 rather than 1 in the submatrix function
submatrix' :: 
    (Int, Int)      -- ^ Rows indices
    -> (Int, Int)   -- ^ Cols indices
    -> M.Matrix a
    -> M.Matrix a
submatrix' (ro,rk) (co,ck) m = submatrix (ro+1) (rk+1) (co+1) (ck+1) m



-- | Lens for columns
colAt :: Int -> Lens' (M.Matrix Rational) Col
colAt j = lens (getCol' j) (\m c -> setCol' j c m)

getCol' :: Int -> M.Matrix a -> M.Matrix a
getCol' idx = colVector . (getCol (idx+1))

setCol' :: Int -> Col -> M.Matrix Rational -> M.Matrix Rational
setCol' idx col mat 
    | idx == 0 = col <|> right
    | idx == (ncols mat) - 1 = left <|> col
    | otherwise = left <|> col <|> right
    where
        left = submatrix' (0,(nrows mat)-1) (0,idx-1) mat
        right = submatrix' (0,(nrows mat)-1) (idx+1, (ncols mat)-1) mat

-- | Lens for rows
rowAt :: Int -> Lens' (M.Matrix Rational) Row
rowAt i = lens (getRow' i) (\m r -> setRow' i r m)

getRow' :: Int -> M.Matrix a -> M.Matrix a
getRow' idx = rowVector . (getRow (idx+1))

setRow' :: Int -> Col -> M.Matrix Rational -> M.Matrix Rational
setRow' idx row mat
    | idx == 0 = row <-> down
    | idx == (nrows mat) - 1 = up <-> row
    | otherwise = up <-> row <-> down
   
    where
        up = submatrix' (0,idx-1) (0,(ncols mat)-1)  mat
        down = submatrix' (idx+1, (nrows mat)-1) (0,(ncols mat)-1) mat

-- | Lens for accessing elements

elemAt :: (Int, Int) -> Lens' (M.Matrix a) a
elemAt (i, j) = lens (getElem' (i,j)) (\m x -> setElem' x (i,j) m) 

getElem' :: (Int, Int) -> M.Matrix a -> a
getElem' (row, col) = getElem (row+1) (col+1)

setElem' :: a -> (Int, Int) -> M.Matrix a -> M.Matrix a
setElem' elem (row, col) mat
    | row < 0 || col < 0 || row >= nrows mat || col >= ncols mat = error ("setElem': Trying to set at (" ++ show row ++ "," ++ show col ++ ") in a " ++ show (nrows mat) ++ "x" ++ show (ncols mat) ++ " matrix.")
    | otherwise = setElem elem (row+1, col+1) mat

mapCol' :: (Int -> a -> a) -> Int -> M.Matrix a -> M.Matrix a
mapCol' f col m
    | col < 0 || col > (ncols m)-1 = error "mapCol': index out of bounds" 
    | otherwise = mapCol f (col+1) m

mapRow' :: (Int -> a -> a) -> Int -> M.Matrix a -> M.Matrix a
mapRow' f row m
    | row < 0 || row > (nrows m)-1 = error "mapCol': index out of bounds" 
    | otherwise = mapRow f (row+1) m




getOptimumVertex :: M.Matrix Rational -> Col -> Maybe Vertex
getOptimumVertex mat col = fmap (map toRational) $ feasNOpt result
    where
        problem = Maximize $ replicate (ncols mat) (-1)
        constraints = Dense $ safeZipWith (:<=:) (map (map fromRational) $ M.toLists mat) (map fromRational $ concat $ M.toLists col)
        result = exact problem constraints (map Free [1..ncols mat])
        feasNOpt (Optimal (_, vertex)) = Just vertex
        feasNOpt _ = Nothing



{- 
    Dictionary form

                p21     p31
    invA_b
                p22     p32

 -}


toHMatrix :: M.Matrix Rational -> HM.Matrix Double
toHMatrix matrix = HM.matrix cols (map (fromRational) (concat $ M.toLists matrix))
    where 
        cols = ncols matrix



findBasis :: (Maybe (M.Matrix Rational), Maybe (M.Matrix Rational)) -> M.Matrix Rational -> Int -> (Maybe (M.Matrix Rational), Maybe (M.Matrix Rational))
findBasis (basic, cobasic) original colNum
    | colNum > rows = (basic, Just ((fromJust cobasic) <|> (submatrix' (0,rows-1) (colNum, (ncols original)-1) original)) )
    | currRank == candRank = findBasis (basic, Just newCobasic) original (colNum + 1)
    | currRank < candRank = findBasis (Just newBasic, cobasic) original (colNum + 1)
    where
        rows = nrows original
        currRank = if isNothing basic then 0 else rank (toHMatrix $ fromJust basic)
        candVec = original ^. colAt colNum
        candMat = if isNothing basic then candVec else (fromJust basic) <|> candVec
        candRank = rank $ toHMatrix candMat

        newBasic = if isNothing basic then candVec else (fromJust basic) <|> candVec
        newCobasic = if isNothing cobasic then candVec else (fromJust cobasic) <|> candVec



sortSystem :: M.Matrix Rational -> (M.Matrix Rational, M.Matrix Rational) 
sortSystem matrix = ((,) <$> (fromJust . fst) <*> (fromJust . snd)) (findBasis (Nothing, Nothing) matrix 0)
-- sortSystem :: M.Matrix Rational -> Col -> Vertex -> (M.Matrix Rational, Col)

-- sortSystem mat col vertex
--     | cols /=  (rows `div` 2) = (,) mat col
--     | otherwise = (trace $ show $ M.fromLists (upper ++ lower)) (,) (M.fromLists (upper ++ lower)) col
--     where
--         matLists = M.toLists mat
        
--         rows = nrows mat
--         cols = ncols mat
--         slackMatr = submatrix' (0,rows-1) (0, rows - cols -2) $ identity rows
--         lower  = getIndependent matLists [] 0 rows slackMatr
--         upper = matLists \\ lower
        
-- getDictionary :: Matrix Rational -> Col -> Vertex -> Dictionary
-- getDictionary _A b vertex = trace ("In dictionary" ++ show (rows,cols, nrows dictionary, ncols dictionary) ++ show newDict) Dict [0..rows] [rows+1..rows+cols] newDict
--     -- trace ("In dictionary" ++ show _A_B)
--     where
--         (newA, newb) = sortSystem _A b vertex
--         rows = nrows newA
--         cols = ncols newA
--         slack = identity rows
--        -- dictionary = newA <|> slack
--         topRow = rowFromList $ 1 : replicate rows 0 ++  replicate cols (1)
--         leftCol = colFromList $ replicate rows 0
--         dictionary = topRow <-> (leftCol <|> slack <|> newA )

--         -- c_B = submatrix' (0,0) (1,rows) topRow
--         -- c_N = submatrix' (0,0) (rows+1, rows+cols) topRow
--         -- _A_B = submatrix' (0,rows-1) (0,rows-1) dictionary
--         -- _A_N = submatrix' (0,rows-1) (rows, rows+cols-1) dictionary
--         _A_B = submatrix' (0,rows) (0,rows) dictionary
--         _A_N = submatrix' (0,rows) (rows+1, rows+cols) dictionary
--         Right invA_B = inverse _A_B

--         p2 = - invA_B |*| _A_N
--         p3 = invA_B |*| newb
--         -- p21 = (c_N - c_B |*| invA_B |*| _A_N)
--         -- p22 = invA_B |*| _A_N
--         -- p31 = -c_B |*| invA_B |*| newb
--         -- p32 = invA_B |*| newb
--         newDict = ((identity (rows+1)) <|> p2 <|> p3)


-- | Constructs initial dictionary given a set of inequalities and an optimum initial vertex
-- getDictionary :: M.Matrix Rational -> Col -> Vertex -> Dictionary
-- getDictionary _A b vertex = Dict [0..rows] [rows+1..rows+cols] newDict
--     where
--         -- (newA, newb) = sortSystem _A b vertex
--         initialDict = _A <|> slack
--         (basic, cobasic) = sortSystem initialDict
--         (newA, newb) = (,) _A b
--         rows = nrows newA
--         cols = ncols newA
--         slack = identity rows
--         dictionary = newA <|> slack
--         topRow = rowFromList $ 1 :  replicate rows 0 ++  replicate cols 1
--         c_B = submatrix' (0,0) (1,rows) topRow
--         c_N = submatrix' (0,0) (rows+1, rows+cols) topRow
--         _A_B = submatrix' (0,rows-1) (0,rows-1) dictionary
--         _A_N = submatrix' (0,rows-1) (rows, rows+cols-1) dictionary
--         Right invA_B = inverse _A_B
--         p21 = (c_N - c_B |*| invA_B |*| _A_N)
--         p22 = invA_B |*| _A_N
--         p31 = -c_B |*| invA_B |*| newb
--         p32 = invA_B |*| newb

--         newDict = ((identity (rows+1)) <|> (p21 <-> p22) <|> (p31 <-> p32))


getDictionary :: M.Matrix Rational -> Col -> Vertex -> Dictionary
getDictionary _A b vertex = Dict [0..rows] [rows+1..rows+cols] newDict
    where

        -- (newA, newb) = sortSystem _A b vertex
        initialDict = _A <|> slack
        (basic, cobasic) = sortSystem initialDict
        (newA, newb) = (,) _A b
        rows = nrows newA
        cols = ncols newA
        slack = identity rows
        -- dictionary = newA <|> slack
        topRow = rowFromList $ 1 :  replicate rows 0 ++  replicate cols 1
        c_B = submatrix' (0,0) (1,rows) topRow
        c_N = submatrix' (0,0) (rows+1, rows+cols) topRow
        _A_B = basic --submatrix' (0,rows-1) (0,rows-1) dictionary
        _A_N = cobasic --submatrix' (0,rows-1) (rows, rows+cols-1) dictionary
        Right invA_B = inverse _A_B
        p21 = (c_N - c_B |*| invA_B |*| _A_N)
        p22 = invA_B |*| _A_N
        p31 = -c_B |*| invA_B |*| newb
        p32 = invA_B |*| newb

        newDict = ((identity (rows+1)) <|> (p21 <-> p22) <|> (p31 <-> p32))


enteringVariable :: Dictionary -> Maybe Int
enteringVariable dictionary
    | null negs = Nothing
    | otherwise = Just $ (fst.head.sort) negs
    where
        cobasic_0 = zip (dictionary^._N) (map (\j -> dictionary^.dict.elemAt (0,j)) (dictionary^._N))
        negs = filter (\(_,value) -> value < 0) cobasic_0

lexMinRatio :: Dictionary -> Int -> Int
lexMinRatio dictionary s
    | dim+1 == rows = 0
    | null indexed_s = 0
    | otherwise = (dictionary ^. _B) !! (fst $ indexed_s !! (fromJust $ elemIndex (minimum ratios) ratios))
    where
        rows = numRows dictionary
        cols = numCols dictionary
        dim = cols - rows - 1
        dictMatrix = dictionary^.dict
        col_s = dictMatrix ^. colAt s
        _D = (dictMatrix ^. colAt (cols-1)) <|> submatrix' (0,rows-1) (0, rows-1) dictMatrix
        slice_s = (concat.M.toLists) $ submatrix' (dim+1, rows-1) (0,0) col_s
        indexed_s = filter (snd.(fmap (>0))) $ safeZipWith (,) [dim+1..rows-1] slice_s 
        sub_D = map (head . M.toLists . (\i -> _D ^. rowAt i) . fst) indexed_s
        ratios = safeZipWith (map $) (map ((flip (/)) . snd) indexed_s) sub_D


-- | Selects entering and leaving variables for the next pivot in the dictionary
selectPivot :: Dictionary -> Maybe (Int, Int)
selectPivot dictionary
    | s == Nothing = Nothing
    | otherwise = Just (r, fromJust s)
    where 
        s = enteringVariable dictionary
        r = lexMinRatio dictionary (fromJust s)

pivot :: Int ->Int -> Dictionary -> Dictionary
pivot r s dictionary = Dict new_B new_N (_E |*| dictMatrix)
    where
        dictMatrix = dictionary ^. dict
        col_s = dictMatrix ^. colAt (s)
        t = fromJust $ elemIndex r (dictionary ^. _B) -- idxR
        idxS = fromJust $ elemIndex s (dictionary ^. _N)
        a_t = col_s ^. elemAt (t,0)
        eta = (mapCol' (\_ x -> -x/a_t) 0 col_s) & elemAt (t,0) .~ (1/a_t)
        _E = (identity (nrows dictMatrix)) & colAt t .~ eta
        new_B = dictionary^._B & element t .~ s
        new_N = dictionary^._N & element idxS .~ r


simplex :: Dictionary -> Dictionary
simplex dictionary
    | idxsToPivot == Nothing = dictionary
    | otherwise = simplex $ uncurry pivot (fromJust idxsToPivot) dictionary
    where
        idxsToPivot = selectPivot dictionary


-- | Reverse search.
reverseRS :: 
        Dictionary 
    ->  Int          -- ^ element in N
    ->  Maybe Int
reverseRS dictionary v
    | conditions == False = Nothing
    | conditions == True = Just u
    where
        dictMatrix = dictionary^.dict
        v_col = dictMatrix ^. colAt (v)
        w_row_0 = dictMatrix ^. rowAt 0
        u = lexMinRatio dictionary v
        i = fromJust $ elemIndex u (dictionary ^. _B)
        w_row_i = mapRow' (\_ x -> (v_col ^. elemAt (0,0))/(v_col ^. elemAt (i,0)) * x) 0 $ dictMatrix ^. rowAt (i)
        diff_ws = (head.M.toLists) $ w_row_0 - w_row_i
        lastCondition = all (>=0) [(diff_ws!!j)| j <- dictionary^._N , j < u]
        conditions = (w_row_0 ^. elemAt (0,v)) > 0  && u /= 0  && lastCondition 




-- reverseRS :: -- reverse with pivot and selectPivot
--     Dictionary -> 
--     Int -> 
--     Maybe Int
-- reverseRS dictionary v
--     | newPivots == Nothing = Nothing
--     | condition == False = Nothing
--     | condition == True = Just u 
--     where
--         u = lexMinRatio dictionary v
--         prev_B = pivot u v dictionary
--         newPivots = selectPivot prev_B
--         condition = fst (fromJust newPivots) == v && (snd (fromJust newPivots)) == u



getVertex :: Dictionary -> Vertex
getVertex dictionary = concat $ M.toLists $ submatrix' (1,dim) (cols-1, cols-1) (dictionary^.dict)
    where
        rows = numRows dictionary
        cols = numCols dictionary
        dim = cols - rows - 1

revSearch :: Dictionary -> [Vertex]
revSearch dictionary@(Dict _B _N dictMatrix) -- = getVertex dictionary : concatMap revSearch pivoted
    | (not.null) possibleRay = possibleRay ++ (concatMap revSearch pivoted)
    | otherwise = getVertex dictionary : concatMap revSearch pivoted
    where
        rows = numRows dictionary
        cols = numCols dictionary
        valid_N = [i | i <- _N, (reverseRS dictionary i) /= Nothing]
        valid_B = map (lexMinRatio dictionary) valid_N
        pivoted = map (\(r, s) ->  pivot r s dictionary) $ zip valid_B valid_N
        possibleRay = hasRay dictionary


-- lexMin :: Dictionary -> Maybe Extremal
-- lexMin dictionary


hasRay :: Dictionary -> [Vertex]
hasRay dictionary = rays
    -- idxsPivot == Nothing = Nothing
    -- r == 0 = Just ray
    -- otherwise = Nothing
    where
        dictMatrix = dictionary ^. dict
        rows = numRows dictionary
        dim = cols - rows - 1
        cols = numCols dictionary
        nonPositive column = all (<=0) ((concat.M.toLists) column)
        colsWithRays = if dim+1 == rows then
                            [dictMatrix^.colAt j | j <- [rows..cols-2]]
                        else [dictMatrix^.colAt j | j <- [rows..cols-2], nonPositive (submatrix' (dim+1,rows-1) (j,j) dictMatrix )]
        rays =  map (concat . M.toLists . (submatrix' (1,dim) (0,0))) colsWithRays


-- | Implementation of Lexicographic Reverse Search
lrs :: M.Matrix Rational -> Col -> Vertex-> [Vertex]
lrs matrix b vertex = (sort.nub) $ revSearch dictionary
    where
        dictionary = getDictionary matrix b vertex


-- | Transforms a V polytope into a H pointed cone
polytope2LiftedCone :: [Vertex] -> (M.Matrix Rational, Col)
polytope2LiftedCone points = (colOnes <|> hMatrix , colZeros)
    where
        nrows = length points
        colOnes = colFromList $ replicate nrows 1
        hMatrix = M.fromLists points
        colZeros = colFromList $ replicate nrows 0

-- | Transforms a V pointed cone into a H polyotpe
liftedCone2polyotpe :: [Vertex] -> (M.Matrix Rational, Col)
liftedCone2polyotpe points = (hMatrix, b)
    where
        b = colFromList $ map (negate.head) points
        hMatrix = M.fromLists $ map tail points 
    
-- | Implementation of Lexicographic Reverse Search for Facet Enumeration
lrsFacetEnumeration :: [IVertex] -> (M.Matrix Rational, Col)
lrsFacetEnumeration ipoints = hPolytope
    where
        points =  map (map toRational) ipoints
        cols = length (points!!0)
        initialVertex = replicate (cols + 1) 0
        hLiftedCone@(matrix, b) = polytope2LiftedCone points
        vLiftedCone = lrs matrix b initialVertex
        hPolytope = liftedCone2polyotpe vLiftedCone

