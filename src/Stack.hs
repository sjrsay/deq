module Stack where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Data.List as List 

import Automata

lrStack r =
  Auto { regs = rs, stts = qs, actv = am, trns = ts }
  where
    rs = [0 .. r-1]
    qs = [0 .. r]
    am = IntMap.fromList [ (i, [0 .. i-1]) | i <- qs ]
    ts = ts1 ++ ts2
      where
      ts1 = List.map (\i -> (i, 0, LFresh, i, i+1)) [0 .. r-1]
      ts2 = List.map (\i -> (i+1, 1, Stored, i, i)) [0 .. r-1]

rlStack r =
  Auto { regs = rs, stts = qs, actv = am, trns = ts }
  where
    rs = [0 .. r-1]
    qs = [0 .. r]
    am = IntMap.fromList [ (i, [r-i .. r-1]) | i <- qs ]
    ts = ts1 ++ ts2
        where
        ts1 = List.map (\i -> (i, 0, LFresh, r-1-i, i+1)) [0 .. r-1]
        ts2 = List.map (\i -> (i+1, 1, Stored, r-1-i, i)) [0 .. r-1]

