module Main where

import System.IO
import System.Environment ( getArgs )
import System.Exit
import Control.Monad
import System.Directory
import Life3D

usageMessage :: String
usageMessage = unlines [ "gol3d-config-gen -- Generate Life 3D configurations"
                       , "usage: gol3d-config-gen x y z"
                       , "    x: size in x of the block"
                       , "    y: size in y of the block"
                       , "    z: size in z of the block"
                       , "  This program will generate a cube of size x by y by z and then find the powerset"
                       , "of the configurations therein, saving each one to a separate file in a folder named"
                       , "'<x>-<y>-<z>-configs'"
                       , ""
                       ]
        

enumerate :: [a] -> [(Integer, a)]
enumerate xs = zip [0..] xs

findCycles :: Ruleset
           -> Int -- minimum size of powerset
           -> Int -- number of states to look ahead into
           -> Vec3i -- size of bounding box to examine
           -> [[Cell]]
findCycles rs psSize k boxSize = map head $ filter isCyclical $ map (take k . map unmapCells . mkStateList rs . fromCells . map mkCellAt) $ filter ((>psSize) . length) $ powerset $ mkCube boxSize

main :: IO ()
main = do
       args <- getArgs
       when (length args < 5) $ do 
                                hPutStrLn stderr usageMessage
                                exitFailure
       let psize = read $ head args  
       let lookahead = read $ (!! 1) args
       let boxSize = (read $ args !! 2, read $ args !! 3, read $ args !! 4)
       fileH <- openFile (show psize ++ '-':show lookahead ++ '-':show boxSize) WriteMode
       hPutRecording fileH $ findCycles standardRuleset psize lookahead boxSize


       
