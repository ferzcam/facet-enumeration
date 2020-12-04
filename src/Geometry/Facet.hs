module Geometry.Facet 


where

import Util
import Geometry.Vertex
import Geometry.LRS (colFromList)

import Data.Matrix hiding (trace)
import Data.List (sort, delete, nub, isSubsequenceOf, find, (\\))
import qualified Data.Map.Strict as MS
import Data.Maybe
import Debug.Trace
import Linear.Matrix (luSolve)
import Control.Lens
import  qualified Data.Vector as V


-- | Module that implements functions for facet enumeration. Based on the artcile written by Yaguang Yang: A Facet Enumeration Algorithm for Convex Polytopes. 


type Branch = [Int]
type Facet = [Int]
type Hyperplane = [Rational]


remove :: Eq a => a -> [a] -> [a]
remove elem = (delete elem).nub

centroid :: [IVertex] -> Vertex
centroid set = let 
                    n = fromIntegral $ length set 
                    fractionalSet = map (map toRational) set 
                in map (/n) (foldr1 (safeZipWith (+)) fractionalSet)

toOrigin :: [IVertex] -> [Vertex]
toOrigin set = let 
                    fractionalSet = map (map toRational) set 
                    center = centroid set
                in map ($-$ center) fractionalSet



computeHyperplane ::
    [Vertex] ->    -- | Set of d d-dimensional vertices.Thus the matrix is square
    (Maybe Hyperplane, Bool)         -- | (Possible hyperplane, Flag indicating in which way the hyperplane was computed)
computeHyperplane [] = (Nothing, False)
computeHyperplane vertices =    if linearSystem /= Nothing 
                                then (linearSystem, False) 
                                else (computeHyperplane' vertices, True)
    where
        linearSystem = fmap V.toList $ solveLS matrix vector
        matrix = fromLists vertices
        vector = V.fromList $ (replicate (length vertices) 1)

computeHyperplane' ::
    [Vertex] ->    -- set of d d-dimensional vertices.Thus the matrix is square
    Maybe Hyperplane         -- hyperplane
computeHyperplane' [] = Nothing
computeHyperplane' vertices
    | length vertices < dim = error "There must be at least d d-dimensional vertices for computing hyperplane"
    | otherwise =  Just $ map (detLU.fromLists.(diff ++).return) ident
    where
        dim = length (head vertices)
        points = take dim vertices
        diff = map ($-$ (head points)) (tail points)
        ident = toLists $ identity dim
        normalize vector = if (snd norm) == 0 then Nothing else Just $ map (/(snd norm)) vector
            where
                norm = foldl (\(pos, value) x -> (pos+1,value + ((-1)^pos)*x)) (1,0) vector 

-- computeHyperplane' ::
--     [Vertex] ->    -- set of d d-dimensional vertices.Thus the matrix is square
--     Maybe Hyperplane         -- hyperplane
-- computeHyperplane' [] = Nothing
-- computeHyperplane' vertices
--     | length vertices < dim = error "There must be at least d d-dimensional vertices for computing hyperplane"
--     | otherwise = Just $ map (detLU.fromLists.(diff ++).return) ident

--     where
--         dim = length (head vertices)
--         points = take dim vertices
--         diff = map ($-$ (head points)) (tail points)
--         ident = toLists $ identity dim

facetsToVertices :: [Facet] -> MS.Map Int Vertex -> [[Vertex]]
facetsToVertices facets dictIndexVertex = map (take dim) facetsByVertices
    where 
        dim = (length.head.head) facetsByVertices
        facetsByVertices = map (map (fromJust . (flip MS.lookup dictIndexVertex))) facets

generateBranches :: Int -> Int -> AdjacencyMatrix -> [Branch]
generateBranches idx dim adjacency = nub $ map sort $ concatMap (goDeep adjacency dim) (map ((++[idx]).return) neighbors) 
    where
        neighbors = [col | col <- [1..(ncols adjacency)], getElem idx col adjacency]

goDeep :: AdjacencyMatrix -> Int -> Branch -> [Branch]
goDeep adjacency dim b@(x:xs)
    | length b == dim = return b
    | otherwise = concatMap (goDeep adjacency dim) $ (remove []) $ map (smartAppend b) neighbors 
        where
            neighbors = [col | col <- [1..(ncols adjacency)], getElem x col adjacency]
            smartAppend list nElem = if elem nElem list then [] else nElem:list


-- -- | Given a set of points of dimension d, we return a list of d points that include all the coordinates.
-- properVertices :: [Vertex] -> [Vertex]
-- properVertices vertices
--     | length vertices < dim = error "properVertices: not enough points"
--     | otherwise = niceVertices vertices (replicate dim False) [] 0
--     where
--         dim = length $ head vertices
--         niceVertices vertxs bools accVertxs pos
--             | and bools = accVertxs ++ (take (dim-(length accVertxs)) (reverse $ sort vertxs))
--             | pos >= dim = error "properVertices.niceVertices: dimension underflow"
--             | chosenPoint == Nothing && (bools!!pos) == False = error "properVertices.niceVertices: dimension underflow"
--             | otherwise = if bools!!pos then 
--                             niceVertices vertxs bools accVertxs (pos+1)
--                         else
--                             niceVertices (delete (fromJust chosenPoint) vertxs) newBools ((fromJust chosenPoint):accVertxs) (pos+1) 
--             where
--                 chosenPoint = find (\v -> v!!pos /= 0) vertxs
--                 varsEnabled = [i | i <- [0..(dim-1)], (fromJust chosenPoint)!!i /= 0]
--                 newBools = foldr (\i acc -> acc & element i .~ True) bools varsEnabled


-- TO CHECK, not complete
isEmbedded :: [IVertex] -> Bool
isEmbedded vertices
    | all (==0) (map last vertices) = True 
    | length vertices <= dim = True
    | otherwise = all (\p -> detLU ((fromLists (p:firstD)) <|> e) == 0 ) rest
    where
        fractionalVertices =  map (map toRational) vertices
        dim = length $ head fractionalVertices
        firstD = take dim fractionalVertices
        rest = fractionalVertices\\firstD
        e = colFromList $ replicate (dim+1) 1


{- 
    facetEnumetation corresponds to algorithm 2.1 of the aforementioned paper.
    checkVertex corresponds to the outer loop 
    checkBranches corresponds to the inner loop

 -}
facetEnumeration :: 
    [IVertex] ->    -- set of vertices (not centered to origin)
    [(Facet, Hyperplane, Rational)]       -- set of hyperplanes ([[a]], [a], a)
facetEnumeration vertices  =  safeZipWith3 (,,) newFacets cleanedHypers b
    where
        uSet = sort $ toOrigin vertices
        center = centroid vertices
        adjacency = adjacencyMatrix uSet
        dictVertexIndex = MS.fromList $ zip uSet [1..]
        dictIndexVertex = MS.fromList $ zip [1..] uSet
        embedded = isEmbedded vertices
        (_,newFacets, newHyperplanes) = foldr (checkVertex dictIndexVertex dictVertexIndex embedded) (adjacency,[], []) uSet
        cleanedHypers = map fromJust $ remove Nothing newHyperplanes
        b = map (succ . (dot center)) cleanedHypers


facetEnumeration' :: 
    [IVertex] ->    -- set of vertices (not centered to origin)
    [([IVertex], Vertex, Rational)]       -- set of hyperplanes ([[a]], [a], a)
facetEnumeration' vertices  =  safeZipWith3 (,,) newFacetsVertex cleanedHypers b
    where
        uSet = sort $ toOrigin vertices
        center = centroid vertices
        adjacency = adjacencyMatrix uSet
        dictVertexIndex = MS.fromList $ zip uSet [1..]
        dictIndexVertex = MS.fromList $ zip [1..] uSet
        dictIndexOriginalVertex = MS.fromList $ zip [1..] (sort vertices)
        embedded = isEmbedded vertices
        (_,newFacets, newHyperplanes) = foldr (checkVertex dictIndexVertex dictVertexIndex embedded) (adjacency,[], []) uSet
        cleanedHypers = map fromJust $ remove Nothing newHyperplanes
        b = map (succ . (dot center)) cleanedHypers
        newFacetsVertex = map (map (\x -> (MS.!) dictIndexOriginalVertex x)) newFacets


checkVertex ::
    MS.Map Int Vertex ->    -- ^ dictionany of indices and vertices
    MS.Map Vertex Int ->    -- ^ dictionany of vertices and indices
    Bool ->                 -- ^ if the polyhedron has lower dimension than the ambient space
    Vertex ->               -- ^ vertex to check
    (AdjacencyMatrix,       -- ^ adjacency matrix 
    [Facet],                 -- ^ set of facets
    [Maybe Hyperplane]) ->        -- ^ set of hyperplanes
    (AdjacencyMatrix, [Facet], [Maybe Hyperplane])  -- ^ resulting tuple (adjacency,facets, hyperplanes)
checkVertex dictIndexVertex dictVertexIndex embedded ui (adjacency, facets, hyperplanes) = joinTuple adjacency $ foldr (checkBranch dictIndexVertex ui embedded) (facets, hyperplanes) branches
    where
        idx = fromJust $ MS.lookup ui dictVertexIndex
        dim = length ui
        facetsUi = [facet | facet <- facets, elem idx facet] -- take facets that include vertex ui
        inFacets = \branch -> any (isSubsequenceOf (sort branch)) facetsUi
        branches = filter (not.inFacets) (generateBranches idx dim adjacency)
        joinTuple a (b,c) = (a,b,c)



checkBranch ::
    MS.Map Int Vertex ->    -- ^ dictionary of indices and vertices
    Vertex ->          -- ^ vertex ui that will be the root
    Bool ->                 -- ^ if the polyhedron has lower dimension than the ambient space
    Branch ->   -- ^ branch under ui
    ([Facet],          -- ^ set of facets
    [Maybe Hyperplane]) ->       -- ^ set of hyperplanes
    ([Facet], [Maybe Hyperplane])  -- ^ resulting tuple (facets, hyperplanes)
checkBranch dictIndexVertex ui embedded branch (facets, hyperplanes)
    | hyper == Nothing = (facets, hyperplanes)
    | otherwise =
        if (not.(elem hyper)) hyperplanes && all (<= pure 1) equation12 && (((fmap dot hyper) <*> (pure ui)) > pure 0) -- || isEmbedded
        then ((complete branch):facets,hyper:hyperplanes)
        else  (facets,hyperplanes)
        where
            branchToVertices = map (\idx -> fromJust $ MS.lookup idx dictIndexVertex) branch
            isTrivialVal = all (==0) (map last branchToVertices) 
            (hyper, computedWithDet) = computeHyperplane branchToVertices
            set = map snd $ MS.toList dictIndexVertex
            equation12 = map (((fmap dot hyper) <*>) . pure) set
            complete br = if computedWithDet then
                                sortBySublist branch (MS.keys dictIndexVertex)
                        else sortBySublist branch (MS.keys $ MS.filter (\v -> ((fmap dot hyper) <*> (pure v)) == pure 1) dictIndexVertex)

                --sortBySublist branch (MS.keys $ MS.filter (\v -> ((fmap dot hyper) <*> (pure v)) == pure 1) dictIndexVertex)
--  if computedWithDet then
--                                 sortBySublist branch (MS.keys dictIndexVertex)
--                         else sortBySublist branch (MS.keys $ MS.filter (\v -> ((fmap dot hyper) <*> (pure v)) == pure 1) dictIndexVertex)



sortBySublist :: (Eq a) => [a] -> [a] -> [a]
sortBySublist [] bigList = bigList
sortBySublist (x:xs) bigList
    | elem x bigList = x : sortBySublist xs (delete x bigList)
    | otherwise = error "sortBySublist: element is not found in bigger list"