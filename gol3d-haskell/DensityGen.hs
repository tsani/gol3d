{- The purpose of this program is to create a configuration with a given cell density, specified as a commandline argument. -}

module DensityGen (makeRandomPatternWithDensity) where

import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stdout, stderr, openFile, IOMode(WriteMode) )
import System.Random ( randomRs, newStdGen, StdGen )
import System.Exit
import Data.Time
import Control.Monad ( when, forever )

import qualified Life3D as L

type Density = Double
type Length = Integer

showUsage = sequence_ $ map (hPutStrLn stderr)
                        [ "DensityGen -- generate a random configuration of cells with a given density"
                        , "  usage: DensityGen <density> <x> <y> <z> <k>"
                        , "         DensityGen -h"
                        , "  The x, y, and z parameters control the size of the bounding box of the pattern."
                        , "  k is how many generations to look into the future by."
                        , "  The resulting configuration is written to stdout."
                        , "  Using the -h switch will cause the program to display this message and exit."
                        ]

main = do
       args <- getArgs
       when (length args /= 5 || head args == "-h") $ showUsage >> exitWith (ExitFailure 1)
       let density = read (head args)
       let xSize = read (args !! 1)
       let ySize = read (args !! 2)
       let zSize = read (args !! 3)
       let k = read (args !! 4)
       mainLoop density k xSize ySize zSize

mainLoop :: Density -> Int -> Length -> Length -> Length -> IO ()
mainLoop density k xSize ySize zSize = forever $ do 
                 rng <- newStdGen
                 let initialCellMap = L.fromCells $ makeRandomPatternWithDensity density xSize ySize zSize rng
                 let icellmapLen = length $ L.unmapCells initialCellMap
                 when (8 <= icellmapLen && icellmapLen <= 12 ) $ do
                     let futureMaps = take k $ L.mkStateList L.standardRuleset initialCellMap
                     when (L.isCyclical $ map L.unmapCells futureMaps) $ do 
                         time <- getCurrentTime
                         fileH <- openFile (show density ++ '-' : 
                                            show xSize ++ '-' : 
                                            show ySize ++ '-' : 
                                            show zSize ++ '-' : 
                                            show time ++ ".pat") WriteMode
                         sequence_ $ map (flip L.hPutPattern (L.unmapCells initialCellMap)) [fileH,stdout]

makeRandomPatternWithDensity :: Density -> Length -> Length -> Length -> StdGen -> L.Pattern
makeRandomPatternWithDensity d xMax yMax zMax rng = map (L.mkCellAt . fst) $ filter (\(pt, pb) -> pb < d) allPointsWithProbs
    where allPointsWithProbs = zip [(x,y,z) | x <- [0..xMax], y <- [0..yMax], z <- [0..zMax]] 
                                   (randomRs (0.0, 1.0) rng)
