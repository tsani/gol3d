{- The purpose of this program is to create a configuration with a given cell density, specified as a commandline argument. -}

module Main where

import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stdout, stderr )
import System.Random ( randomRs, newStdGen, StdGen )
import System.Exit

import Control.Monad ( when )

import qualified Life3D as L

showUsage = sequence_ $ map (hPutStrLn stderr)
                        [ "DensityGen -- generate a random configuration of cells with a given density"
                        , "  usage: DensityGen <density> <x> <y> <z>"
                        , "         DensityGen -h"
                        , "  The x, y, and z parameters control the size of the bounding box of the pattern."
                        , "  The resulting configuration is written to stdout."
                        , "  Using the -h switch will cause the program to display this message and exit."
                        ]

main = do
       args <- getArgs
       when (length args /= 4 || head args == "-h") $ showUsage >> exitWith (ExitFailure 1)
       let density = read (head args)
       let xSize = read (args !! 1)
       let ySize = read (args !! 2)
       let zSize = read (args !! 3)
       rng <- newStdGen
       let pat = makeRandomPattern density xSize ySize zSize rng
       L.hPutPattern stdout pat 

makeRandomPattern :: Double -> Integer -> Integer -> Integer -> StdGen -> L.Pattern
makeRandomPattern d xMax yMax zMax rng = map (L.mkCellAt . fst) $ filter (\(pt, pb) -> pb < d) allPointsWithProbs
    where allPointsWithProbs = zip [(x,y,z) | x <- [0..xMax], y <- [0..yMax], z <- [0..zMax]] 
                                   (randomRs (0.0, 1.0) rng)
