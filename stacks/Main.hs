{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.IntMap as IntMap
import System.Console.CmdArgs.Implicit ((&=),summary,help,cmdArgs)
import System.CPUTime
import Text.Printf

import Bisim
import Automata
import Stack

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
  } &= summary "DRAEquiv: Equivalence testing for deterministic register automata."

main :: IO ()
main = 
  do  args <- cmdArgs argSpec
      let lr = lrsz args
      let rl = rlsz args
      let (sumAuto, qTrans) = Automata.sum (Stack.lrStack lr) (Stack.rlStack rl)
      start <- getCPUTime
      let b = Bisim.raBisim sumAuto (0, IntMap.fromList [], qTrans 0)
      putStr $ show lr ++ ", " ++ show rl ++ ", " ++ show b ++ ", "
      stop <- getCPUTime
      let diff = (fromIntegral (stop - start)) / (10^9)
      printf "%.0f\n" (diff :: Double)
