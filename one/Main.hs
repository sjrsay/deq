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

one r =
  Auto { regs = rs, stts = qs, actv = am, trns = ts }
  where
    rs = [1..r]
    qs = [1]
    am = IntMap.fromList [(1,[1..r])]
    ts = freshTrans ++ nonFreshTrans
    freshTrans = [ (1, i, LFresh, i, 1) | i <- [1..r]]
    nonFreshTrans = [ (1, i, Stored, j, 1) | i <- [1..r], j <- [1..r]]

data Args =
  Args {
    n :: Int
  }
  deriving (Show,Data,Typeable)

argSpec = 
  Args { 
    n = 1 &= help "registers of size VALUE (default 1)"
  } &= summary "DRAEquiv: Equivalence testing for deterministic register automata."

main :: IO ()
main = 
  do  args <- cmdArgs argSpec
      let r = n args
      let (sumAuto, qTrans) = Automata.sum (one r) (one r)
      start <- getCPUTime
      let b = Bisim.raBisim sumAuto (1, IntMap.fromList [], qTrans 1)
      putStr $ show r ++ ", " ++ show b ++ ", "
      stop <- getCPUTime
      let diff = (fromIntegral (stop - start)) / (10^9)
      printf "%.0f\n" (diff :: Double)