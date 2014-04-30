module Main where

import System.Environment ( getArgs)
import System.IO
import System.Exit

import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Life3D as L3D

subs :: [a] -> [[a]]
subs xs = subs' [] xs
    where subs' :: [a] -> [a] -> [[a]]
          subs' _ []         = []
          subs' prevs (y:ys) = (prevs ++ ys) : subs' (prevs ++ [y]) ys

findSS :: L3D.Ruleset
       -> Int -- depth of evolution
       -> Int -- depth of removal
       -> L3D.CellMap -- current node
       -> State (M.Map L3D.CellMap Bool) ()
findSS _ _ 0 _ = return ()
findSS rs k n cm = do
                   tab <- get
                   case M.lookup cm' tab of
                       Just b -> return ()
                       Nothing -> sequence_ $ map (\cs -> modify $ M.insert cs $ isCycle) states
                   sequence_ $ map (findSS rs k (n-1)) subStates

    where cm'       = L3D.rebaseCellMap cm
          states    = take k $ L3D.mkStateList rs cm
          subStates = map L3D.fromCells $ subs $ L3D.unmapCells cm'
          isCycle   = L3D.isCyclical $ map L3D.unmapCells states

usageMessage = unlines [ "dfs3d -- find spaceships!"
                       , "usage: dfs3d k n x y z"
                       , "  where k: depth of evolution"
                       , "        n: depth of pruning"
                       , "    x,y,z: dimensions of bounding box"
                       , "  Cyclical configurations found are written to stdout in a recording format."
                       ]

main :: IO ()
main = do
       args <- getArgs
       when (length args < 5) $ hPutStrLn stderr usageMessage >> exitFailure

       let evDepthS  = args !! 0
       let pDepthS   = args !! 1
       let xS        = args !! 2
       let yS        = args !! 3
       let zS        = args !! 4

       L3D.hPutRecording stdout $ ss (read evDepthS) (read pDepthS) (read xS) (read yS) (read zS)
    where ss k n x y z = let (_, cmap) = runState (findSS L3D.standardRuleset k n (L3D.fromCells $ map L3D.mkCellAt $ L3D.mkCube (x,y,z))) (M.fromList [])
                         in map (L3D.unmapCells . fst) $ filter snd $ M.toList cmap

