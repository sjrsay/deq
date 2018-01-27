{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.IntMap as IntMap
import System.Console.CmdArgs.Implicit ((&=),summary,help,cmdArgs)

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
      let (sumAuto, qTrans) = Automata.sum (Stack.lrStack $ lrsz args) (Stack.rlStack $ rlsz args)
      let b = Bisim.raBisim sumAuto (0, IntMap.fromList [], qTrans 0)
      putStrLn $ "LR Stack and RL Stack are bisimilar? " ++ show b
