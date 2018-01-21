module Automata where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Data.List as List

type Reg = Int
type Tag = Int
type State = Int
type StateMap a = IntMap a

data Mode = Stored | LFresh | GFresh
  deriving (Eq,Show)

type Transition = (State, Tag, Mode, Reg, State)

init (q,_,_,_,_) = q
tag  (_,t,_,_,_) = t
mode (_,_,m,_,_) = m
indx (_,_,_,i,_) = i
term (_,_,_,_,q) = q

-- | Deterministic register automata.
data Auto =
  Auto {
    regs :: [Reg],
    -- ^ The registers over which the automaton operates, ordered.
    stts :: [State],
    -- ^ The control states of the automaton.
    actv :: IntMap [Reg],
    -- ^ The active registers at each state, ordered.
    trns :: [Transition]
    -- ^ The transition function of the automaton as a list of transitions.
  }
  deriving (Eq,Show)

sum :: Auto -> Auto -> (Auto, State -> State)
sum a1 a2 =
  (Auto { regs = rs, stts = qs, actv = am, trns = ts }, \q -> q+mq)
  where 
    mq = List.maximum (stts a1) + 1
    shiftQ q = q + mq
    shiftT (q1, tag, mode, reg, q2) = (q1 + mq, tag, mode, reg, q2 + mq)
    rs = List.sort (List.nub (regs a1 ++ regs a2))
    qs = stts a1 ++ List.map shiftQ (stts a2)
    am = IntMap.union (actv a1) (IntMap.mapKeysMonotonic shiftQ $ actv a2)
    ts = trns a1 ++ List.map shiftT (trns a2)