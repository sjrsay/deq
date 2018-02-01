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

global b r =
  Auto { regs = rs, stts = qs, actv = am, trns = ts }
  where
    rs = [1..r]
    qs = [0..r+1]
    am = IntMap.fromList $ (r+1,[1..r]) : [(i,[1..i]) | i <- [0..r]]
    ts = lastTrans:freshTrans
    lastTrans = (r, 1, if b then LFresh else GFresh, 1, r+1)
    freshTrans = [ (i, 1, GFresh, i+1, i+1) | i <- [0..r-1]]

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
      let (sumAuto, qTrans) = Automata.sum (global True r) (global False r)
      start <- getCPUTime
      let b = Bisim.fraBisim sumAuto (0, IntMap.fromList [], qTrans 0)
      putStr $ show r ++ ", " ++ show b ++ ", "
      stop <- getCPUTime
      let diff = (fromIntegral (stop - start)) / (10^9)
      printf "%.0f\n" (diff :: Double)