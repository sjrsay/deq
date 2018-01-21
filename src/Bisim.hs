module Bisim where


import Prelude hiding (init)

import qualified Data.Maybe as Maybe

import Data.IntMap (IntMap,(!))
import qualified Data.IntMap as IntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.List ((\\))
import qualified Data.List as List

import Data.Sequence (Seq(..),ViewL(..),(><))
import qualified Data.Sequence as Seq

import Automata 
import PPerm 
import GenSystem 

succs :: Auto -> (State, PPerm, State) -> Maybe [(State, PPerm, State)]
-- ^ @succs a (q1, s, q2)@ is @Just pps@ whenever @q1@ and @q2@ are one-step @s@-bisimilar
--   in @a@ with successor triples @pps@ and otherwise @Nothing@.
succs a (q1,s,q2) =
  do q1succs <- mapM (trans id s q2) (List.filter ((==) q1 . init) (trns a))
     q2succs <- mapM (trans reflect (inverse s) q1) (List.filter ((==) q2 . init) (trns a))
     return (concat q1succs ++ concat q2succs)
  where

    reflect (q1, s, q2) = (q2, inverse s, q1) 
    hasInitTagMode q t m (q',t',m',_,_) = q == q' && t == t' && m == m'
    hasInitTagModeIndex q t m i (q',t',m',i',_) = q == q' && t == t' && m == m' && i == i'
    
    -- The conditions on successors are given, as in the draft, for q1 and then we obtain
    -- the symmetric conditions for q2 by using reflect.
    -- If there exists q1 --t,i--> q1' and i in dom(s) then...
    trans r s q2 (q1,t,Stored,i,q1') | Just j <- IntMap.lookup i s =
      do (_,_,_,_,q2') <- List.find (hasInitTagModeIndex q2 t Stored j) (trns a)
         return [r (q1', s, q2')]
    -- If there exists q1 --t,i--> q1' and i notin dom(s) then...
    trans r s q2 (q1,t,Stored,i,q1') | Nothing <- IntMap.lookup i s =
      do (_,_,_,j,q2') <- List.find (hasInitTagMode q2 t LFresh) (trns a)
         return [r (q1', remap i j s, q2')]
    -- If there exists q1 --t,i*--> q1' then...
    trans r s q2 (q1,t,LFresh,i,q1') =
      do (_,_,_,j,q2') <- List.find (hasInitTagMode q2 t LFresh) (trns a)
         let notRng = actv a ! q2 \\ rng s
         let find k = List.find (hasInitTagModeIndex q2 t Stored k) (trns a)
         let mkSucc k =
               do (_,_,_,_,q2') <- find k
                  return (r (q1', remap i k s, q2'))
         succs <- mapM mkSucc notRng
         return (r (q1', remap i j s, q2') : succs)



bisim :: Auto -> (State, PPerm, State) -> Bool
-- ^ @bisim a regs pp@ is true just if every pair of configurations 
--   represented by @pp@ are bisimilar in @a@.
bisim a pp =
  loop (Seq.singleton pp) (initGs a)
  where

    initGs :: Auto -> GenSystem
    initGs a = GenSys {
        rep = IntMap.fromList (List.map (\q -> (q, q)) $ stts a),
        chr = IntMap.fromList (List.map (\q -> (q, actv a ! q)) $ stts a),
        grp = IntMap.fromList (List.map (\q -> (q, [])) $ stts a),
        ray = IntMap.fromList (List.map (\q -> (q, toPPerm (regs a) 1)) $ stts a)
      }

    loop :: Seq (State, PPerm, State) -> GenSystem -> Bool
    loop qu gs =
      case Seq.viewl qu of
        EmptyL           -> True
        (q1,s,q2) :< que ->
          if isMember gs (q1,s,q2) then
            loop que gs
          else
            case succs a (q1,s,q2) of
              Nothing  -> False
              Just pps ->
                let que' = Seq.fromList pps in
                let gs'  = extend (q1,s,q2) gs
                in loop (que >< que') gs'
