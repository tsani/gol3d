module Main where

import qualified Life3D as L
import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )
import Control.Monad ( when, sequence_ )
import System.Exit

showUsage = sequence_ $ map (hPutStrLn stderr)
          [ "density-finder: find the live cell density of a pattern."
          , "  usage: density-finder [-h]"
          , "  The default behaviour is to read a pattern from stdin and print its density."
          , "  The -h switch will cause the program to print this message and exit."
          ]

showHelpAndExit = showUsage >> exitWith (ExitFailure 1)

main = do
       args <- getArgs
       when (length args > 0) showHelpAndExit
       input <- getContents
       putStrLn $ show $ L.patternDensity $ L.fromPattern input
