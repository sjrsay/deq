module Automata where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.List as List
import qualified Data.Text as T

type Reg = Int
type Tag = Int
type State = Int
type StateMap a = IntMap a

data Mode = Stored | LFresh | GFresh
  deriving (Eq,Read,Show)

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
    trns :: [Transition],
    -- ^ The transition function of the automaton as a list of transitions.
    tagn :: Bimap T.Text Int,
    -- ^ The given names of the letters of the finite alphabet
    sttn :: Bimap T.Text Int
    -- ^ The given names of the states
  }
  deriving (Eq,Show)

reqGlobalFreshness :: Auto -> Bool 
reqGlobalFreshness a =
  List.any (\(_,_,m,_,_) -> m == GFresh) (trns a)

sum :: Auto -> Auto -> (Auto, State -> State, State -> State)
-- ^ @sum a1 a2@ is a triple @(a, inl, inr)@ in which @a@ is the disjoint union of the two 
--   input automata @a1@ and @a2@ and @inl@ and @inr@ are the injections that witness how
--   states of @a1@ and @a2@ are mapped to states of @a@ respectively.
sum a1 a2 =
  (Auto { regs = rs, stts = qs, actv = am, trns = ts, tagn = tagn', sttn = sttn' }, id, \q -> q+mq)
  where 
    mq = List.maximum (stts a1) + 1
    shiftQ q = q + mq
    shiftT (q1, tag, mode, reg, q2) = (q1 + mq, tag, mode, reg, shiftQ q2)
    rs = List.sort (List.nub (regs a1 ++ regs a2))
    qs = stts a1 ++ List.map shiftQ (stts a2)
    am = IntMap.union (actv a1) (IntMap.mapKeysMonotonic shiftQ $ actv a2)
    ts = trns a1 ++ List.map shiftT (trns a2)
    tagn' = tagn a2
    sttn' = List.foldr (\(txt,q) bm -> Bimap.insert txt (shiftQ q) bm) (sttn a1) (Bimap.toList (sttn a2))