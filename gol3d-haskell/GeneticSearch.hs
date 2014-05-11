{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.IO
import System.Random
import Control.Monad
import Data.List
import Life3D
import qualified Data.Map as M
import qualified DensityGen as DGen
import qualified Data.Vector as V

type Fitness = Double

newtype FitnessPattern = FitnessPattern { getPattern :: Pattern }

fitness :: Int -> Pattern -> (Pattern, Fitness)
fitness k p = (nextP, fromIntegral (length nextP) / fromIntegral (length p))
    where nextP = patList !! k
          patList = map unmapCells $ mkStateList standardRuleset (fromCells p)

-- a handy shortcut, since it's all we really want, it turns out.
fitness' = snd . fitness 1

instance Eq FitnessPattern where
    FitnessPattern p1 == FitnessPattern p2 = fitness' p1 == fitness' p2

instance Ord FitnessPattern where
    FitnessPattern p1 `compare` FitnessPattern p2 = fitness' p1 `compare` fitness' p2

breed :: Int -> Int -> (Pattern, Pattern) -> (Pattern, Pattern)
breed cut1 cut2 (p1, p2) = (take cut1 smallerPattern ++ take cutSz (drop cut1 biggerPattern) ++ drop cut2 smallerPattern,
                  take cut1 biggerPattern ++ take cutSz (drop cut1 smallerPattern) ++ drop cut2 biggerPattern)
    where (smallerPattern, biggerPattern) = if length p1 < length p2 then (p1,p2) else (p2,p1)
          --(cut1, cut2) = let l = length smallerPattern in (l `div` 3, 2 * l `div` 3)
          cutSz = cut2 - cut1
main = do
       rngs <- sequence $ replicate 100 newStdGen
       -- TODO make those constants command-line arguments / move this stuff to another function
       let pats = map (DGen.makeRandomPatternWithDensity (0.17) 10 10 10) rngs

--mainLoop :: [Pattern] -> IO ()
--mainLoop pats = do

fitnessSum :: [Pattern] -> Fitness
fitnessSum = foldr ( (+) . fitness' ) 0

rouletteWheel :: 



       -- need to generate 1000 random patterns with a given density (see DensityGen.hs)
       -- then sort by fitness, save the 1% most elite. Split the rest into pairs, and breed.
       -- Resort the entire list by fitness and reconsider who is elite.
       -- Make sure to drop patterns with no live cells.
       -- Drop patterns with less than 0.2 fitness.

