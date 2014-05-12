{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.IO
import System.Random
import System.Environment ( getArgs )
import Control.Monad
import Control.Monad.State
import Data.List
import Life3D
import qualified Data.Map as M
import qualified DensityGen as DGen
import qualified Data.Vector as V

import Debug.Trace

type Fitness = Double

newtype FitnessPattern = FitnessPattern { getPattern :: Pattern }

fitness :: Int -> Pattern -> (Pattern, Fitness)
fitness k p = (nextP, (**2) $ fromIntegral (length nextP) / fromIntegral (length p))
    where nextP = patList !! k
          patList = map unmapCells $ mkStateList standardRuleset (fromCells p)

-- a handy shortcut, since it's all we really want, it turns out.
fitness' = snd . fitness 1

instance Eq FitnessPattern where
    FitnessPattern p1 == FitnessPattern p2 = fitness' p1 == fitness' p2

instance Ord FitnessPattern where
    FitnessPattern p1 `compare` FitnessPattern p2 = fitness' p1 `compare` fitness' p2

breed' :: Int -> Int -> (Chromosome, Chromosome) -> (Chromosome, Chromosome)
breed' cut1 cut2 (p1, p2) = (p1beg ++ p2mid ++ p1end, p2beg ++ p1mid ++ p2end)
    where ((p1beg, p1rest), (p2beg, p2rest)) = (splitAt cut1 p1, splitAt cut2 p2)
          ((p1mid, p1end), (p2mid, p2end)) = (splitAt (cut2-cut1) p1rest, splitAt (cut2-cut1) p2rest)

breed :: Int -> Int -> (Pattern, Pattern) -> (Pattern, Pattern)
breed cut1 cut2 (p1, p2) = (map mkCellAt $ fromChromosome c1 bbox1, map mkCellAt $ fromChromosome c2 bbox2)
    where (p1', p2') = (map cellPosition p1, map cellPosition p2)
          (bbox1, bbox2) = (boundingBox p1', boundingBox p2')
          (c1, c2) = breed' cut1 cut2 (toChromosome p1', toChromosome p2')

main = do
       args <- getArgs
       let d = read $ head args
       rngs <- sequence $ replicate 25 newStdGen
       -- TODO make those constants command-line arguments / move this stuff to another function
       let pats = map (DGen.makeRandomPatternWithDensity d 7 7 7) rngs
       rng <- newStdGen
       mainLoop pats rng

mainLoop :: [Pattern] -> StdGen -> IO ()
mainLoop pats rng = do
                    let (rng', (parents, others)) = rouletteWheel pats rng
                    let smallerL = if (length . fst) parents > (length . snd) parents 
                                   then (length . snd) parents else (length . fst) parents
                    let (cut1', rng'') = randomR (0, smallerL) rng'
                    let (cut2', rng''') = randomR (0, smallerL) rng''
                    let (cut1, cut2) = if cut1' < cut2' then (cut1', cut2') else (cut2', cut1')
                    let (c1, c2) = breed cut1 cut2 parents
                    putStrLn $ show $ (fitness' c1, fitness' c2)
                    mainLoop (c1:c2:others) rng'''

fitnessSum :: [Pattern] -> Fitness
fitnessSum = foldr ( (+) . fitness' ) 0

rouletteWheel :: [Pattern] -> StdGen -> (StdGen, ((Pattern, Pattern), [Pattern]))
rouletteWheel ps rng = let (cs, theRest) = getPatInRange x1 x2 
                           (r1, r2) = if (not . null . tail) cs 
                                      then (head cs, (head . tail) cs) else (head cs, head cs)
                           in (rng'', ((r1,r2), theRest))
    where withFitness ps = zip ps (map fitness' ps)
          sumFitness = fitnessSum ps
          roulette :: [(Pattern, (Double, Double))]
          roulette = evalState (makeRoulette $ withFitness ps) 0
          (x1, rng') = randomR (0, sumFitness) rng
          (x2, rng'') = randomR (0, sumFitness) rng'
          getPatInRange :: Double -> Double -> ([Pattern],[Pattern])
          getPatInRange x1 x2 = let (l, r) = partition (\(_, (beg,end)) -> beg <= x1 && x1 <= end || beg <= x2 && x2 <= end) roulette in (map fst l, map fst r)

makeRoulette :: [(Pattern, Fitness)] -> State Double [(Pattern, (Double, Double))]
makeRoulette [] = return []
makeRoulette ((p,f):pfs) = do
                        xi <- get
                        let xf = xi + f
                        put xf
                        pfs' <- makeRoulette pfs
                        return $ (p,(xi,xf)) : pfs'



       -- need to generate 1000 random patterns with a given density (see DensityGen.hs)
       -- then sort by fitness, save the 1% most elite. Split the rest into pairs, and breed.
       -- Resort the entire list by fitness and reconsider who is elite.
       -- Make sure to drop patterns with no live cells.
       -- Drop patterns with less than 0.2 fitness.

