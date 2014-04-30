module Life3D where

import qualified Data.Map.Strict as M
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad.State
import System.Random
import System.IO

type Vec3i  = (Integer, Integer, Integer)
type CellMap = M.Map Vec3i Cell
type Pattern = [Cell]
type Recording = [Pattern]
type NeighbourMatrix = M.Map Vec3i Integer

vec3add (x1, x2, x3) (y1, y2, y3) = (x1 + y1, x2 + y2, x3 + y3) -- vector addition
vec3mult k (x1, x2, x3)           = (k * x1, k * x2, k * x3)
vec3negate (x1, x2, x3)           = (-x1, -x2, -x3)
vec3subtract x y                  = x `vec3add` vec3negate y

data Cell = Cell { cellAge      :: Integer
                 , cellPosition :: Vec3i
                 } 

type Rand a = StdGen -> (a, StdGen)

instance Eq Cell where
    Cell _ p1 == Cell _ p2 = p1 == p2

instance Ord Cell where
    compare c1 c2 = compare (cellPosition c1) (cellPosition c2)

instance Show Cell where
    show (Cell age (x1, x2, x3) ) = show x1 ++ ' ' : show x2 ++ ' ' : show x3 ++ ' ' : show age

data Ruleset = Ruleset Integer Integer Integer Integer
data SurvivalResult = Survival | Death | Birth

-- |The standard ruleset for 3D Game of Life, which supports analogous gliders and blinkers, among other interesting things.
standardRuleset = Ruleset 6 6 7 5

-- |All 26 vectors that need to be added to a given point to produce its regular, cubic-lattice neighbours.
neighbourOffsets :: [Vec3i]
neighbourOffsets = [(-1,-1,0),(0,-1,0),(1,-1,0),(-1,0,0),(1,0,0),(-1,1,0),(0,1,0),(1,1,0)
                   ,(0,-1,-1),(0,-1,1),(0,0,-1),(0,0,1),(0,1,-1),(0,1,1)
                   ,(-1,0,-1),(1,0,-1),(-1,0,1),(1,0,1)
                   ,(-1,-1,-1),(1,-1,-1),(1,-1,1),(-1,-1,1),(-1,1,-1),(-1,1,1),(1,1,-1),(1,1,1)
                   ]

recordingDelimiter :: String
recordingDelimiter = "---"

rebasePosition :: Vec3i -> Vec3i -> Vec3i
rebasePosition (x1, x2, x3) (y1, y2, y3) = (min x1 y1, min x2 y2, min x3 y3)

rebaseCells :: [Cell] -> [Cell]
rebaseCells [] = []
rebaseCells cs = map (\c -> c { cellPosition = cellPosition c `vec3subtract` newBase }) cs
    where newBase = foldl (\p0 (Cell _ p') -> rebasePosition p0 p') (cellPosition $ head cs) cs

rebaseCellMap :: CellMap -> CellMap
rebaseCellMap = M.fromList . map (\c -> (cellPosition c, c)) . rebaseCells . map (\(_, c) -> c) . M.toList

-- |Creates a new Cell (age = 1) at a given location.
mkCellAt :: Vec3i -> Cell
mkCellAt pos = Cell { cellAge      = 1
                    , cellPosition = pos
                    }

mkCube :: Vec3i -> [Vec3i]
mkCube (x, y, z) = do
    xs <- [0..x-1]
    ys <- [0..y-1]
    zs <- [0..z-1]
    return (xs, ys, zs)

mkSoup :: Double -> Vec3i -> Rand [Vec3i]
mkSoup density cubeDim r = foldr randomPrune ([], r) fullCube
    where fullCube :: [Vec3i]
          fullCube = mkCube cubeDim
          randomPrune :: Vec3i -> ([Vec3i], StdGen) -> ([Vec3i], StdGen)
          randomPrune p (ps, rng) = let (prob, rng') = random rng
                                    in if prob <= density
                                       then (p:ps, rng')
                                       else (ps, rng')
          

fromCells :: Pattern -> CellMap
fromCells = M.fromList . map (\c -> (cellPosition c, c))

-- |Parses a String into a Cell. An age is optional.
cellFromString :: String -> Cell
cellFromString str = let x = read (ws !! 0)
                         y = read (ws !! 1)
                         z = read (ws !! 2)
                     in Cell { cellPosition = (x, y, z)
                             , cellAge      = age
                             }
                 where ws  = words str
                       age = if length ws == 4 then read (ws !! 3) else 1

-- |Updates a Cell, simply increasing its age by one.
updateCell :: Cell -> Cell
updateCell c = Cell { cellAge      = cellAge c + 1
                    , cellPosition = cellPosition c
                    }

-- |Determines the effect of having a certain number of neighbours in a given Ruleset.
survivalResultFor :: Ruleset -> Integer -> SurvivalResult
survivalResultFor (Ruleset birthMin birthMax liveMax liveMin) n
    | n >= birthMin && n <= birthMax = Birth
    | n >  liveMax  || n <  liveMin  = Death
    | otherwise                      = Survival

-- |Produces the list of positions that neighbour on a given Cell.
cellNeighbourPositions :: Cell -> [Vec3i]
cellNeighbourPositions (Cell _ pos) = map (vec3add pos) neighbourOffsets

-- |Produces a Map associating positions with the number of live neighbours that location has.
neighbourCountMatrix :: Pattern -> NeighbourMatrix
neighbourCountMatrix cs = foldl (\mp  c -> 
                                 foldl (\mp' p -> M.insertWithKey (\_ -> (+) ) p 1 mp') mp (cellNeighbourPositions c)
                                ) 
                                (M.fromList []) cs

repopulateFromNeighbourCounts :: CellMap -> NeighbourMatrix -> Ruleset -> CellMap
repopulateFromNeighbourCounts cm ncs rs = M.fromList $ foldr (\(pos, count) cellList ->
                                              case survivalResultFor rs count of
                                                  Birth -> case M.lookup pos cm of
                                                                  Just cell -> (pos, updateCell cell) : cellList
                                                                  Nothing -> (pos, mkCellAt pos) : cellList 
                                                  Death -> cellList
                                                  Survival -> case M.lookup pos cm of
                                                                  Just cell -> (pos, updateCell cell) : cellList
                                                                  Nothing -> cellList 
                                             ) [] (M.toList ncs)

updateCellMap :: Ruleset -> CellMap -> CellMap
updateCellMap rs cm = repopulateFromNeighbourCounts cm (neighbourCountMatrix (map (\(_, p) -> p) $ M.toList cm)) rs

mkStateList :: Ruleset -> CellMap -> [CellMap]
mkStateList rs cm = cm : unfoldr (\c -> if M.size c > 0 then let c' = updateCellMap rs c in Just (c', c') else Nothing) cm

unmapCells :: CellMap -> Pattern
unmapCells = map snd . M.toList

patternDensity :: Pattern -> Double
patternDensity cs = (fromIntegral $ length cs)
                  /(fromIntegral $ M.size (neighbourCountMatrix cs)) 

-- |Checks whether the sequence returns to its first state.
isCyclical :: [Pattern] -> Bool
isCyclical (cs:css) = any (isomorphic cs') $ map rebaseCells css
    where cs' = rebaseCells cs

isomorphic :: Eq a => [a] -> [a] -> Bool -- I'm pretty sure that the reverse check is unnecessary because there being duplicates is not normally possible
isomorphic cm1 cm2 = length cm1 == length cm2 && allContains cm1 cm2 && flip allContains cm2 cm1
    where allContains :: Eq a => [a] -> [a] -> Bool
          allContains cs1 cs2 = all (`elem` cs2) cs1

hPutRecording :: Handle -> Recording -> IO ()
hPutRecording fileH rec = hPutStrLn fileH recStr
    where recStr = recordingToString rec

hPutPattern :: Handle -> Pattern -> IO ()
hPutPattern h pat = hPutStrLn h patStr
    where patStr = patternToString pat

recordingToString :: Recording -> String
recordingToString rec = unlines $ intersperse recordingDelimiter $ concatMap (lines . patternToString) rec

patternToString :: Pattern -> String
patternToString pat = unlines $ map show pat

fromPattern :: String -> Pattern
fromPattern str = map cellFromString str'
    where str' = filter (not . null) $ lines str

fromRecording :: String -> Recording
fromRecording ss = map fromPattern ss'
    where ss' = splitOn recordingDelimiter ss

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
                 where xss = powerset xs

(/\/) :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)

