module Automata where

import Data.IntMap (IntMap)

type Reg = Int
type Tag = Int
type State = Int
type StateMap a = IntMap a

data Mode = Stored | LFresh | GFresh
  deriving Eq

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
    trns :: [Transition]
    -- ^ The transition function of the automaton as a list of transitions.
  }