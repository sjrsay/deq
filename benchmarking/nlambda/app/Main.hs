{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Main where

import GHC.Generics (Generic)
import NLambda
import Prelude hiding (map,sum)
import Control.DeepSeq
import Text.Printf
import System.CPUTime

import Data.Data (Data)
import Data.Typeable (Typeable)
import System.Console.CmdArgs.Implicit ((&=),summary,help,cmdArgs)

-- Adapted from https://github.com/Jaxan/nominal-lstar/blob/master/src/Examples/Stack.hs

data DataInput = Put Atom | Get Atom
  deriving (Eq, Ord, Show, Read, Generic, NominalType, Contextual)

-- The automaton: States consist of stacks and a sink state.
-- The parameter n is the bound.
lrStack :: Int -> Automaton (Maybe [Atom]) DataInput
lrStack n = automatonWithTrashCan
    -- states
    allStates
    -- alphabet
    (map Put atoms `union` map Get atoms)
    -- transitions
    (triplesWithFilter (\s1 a s2 -> maybeIf ((a:s1) `eq` s2) (s1, Put a, s2)) allStates atoms allStates
        `union` triplesWithFilter (\s1 a s2 -> maybeIf (s1 `eq` (a:s2)) (s1, Get a, s2)) allStates atoms allStates)
    -- initial states
    (singleton [])
    -- final states
    allStates
    where
        allStates = sum . fromList $ [states i | i <- [0..n]]
        states i = replicateDifferentAtoms i

rlStack :: Int -> Automaton (Maybe [Atom]) DataInput
rlStack n = automatonWithTrashCan
    -- states
    allStates   
    -- alphabet
    (map Put atoms `union` map Get atoms)
    -- transitions
    (triplesWithFilter (\s1 a s2 -> maybeIf ((s1 ++ [a]) `eq` s2) (s1, Put a, s2)) allStates atoms allStates
        `union` triplesWithFilter (\s1 a s2 -> maybeIf (s1 `eq` (s2 ++ [a])) (s1, Get a, s2)) allStates atoms allStates)
    -- initial states
    (singleton [])
    -- final states
    allStates
    where
        allStates = sum . fromList $ [states i | i <- [0..n]]
        states i = replicateDifferentAtoms i

data Args =
  Args {
    lrsz :: Int, 
    rlsz :: Int
  }
  deriving (Show,Data,Typeable)

argSpec = 
  Args { 
    lrsz = 1 &= help "LR stack of size VALUE (default 1)",
    rlsz = 1 &= help "RL stack of size VALUE (default 1)"
  } &= summary "LR vs RL stack equivalence using NLambda."

main :: IO ()
main =
  do args <- cmdArgs argSpec
     let lr = lrsz args
     let rl = rlsz args
     let a1 = lrStack lr 
     let a2 = rlStack rl 
     start <- getCPUTime
     let f = equivalentDA a1 a2
     putStr $ show lr ++ ", " ++ show rl ++ ", " ++ show f ++ ", "
     stop <- getCPUTime
     let diff = (fromIntegral (stop - start)) / (10^9)
     printf "%.0f\n" (diff :: Double)
     
