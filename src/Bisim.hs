module Bisim where


import Prelude hiding (init)

import qualified Data.Maybe as Maybe

import Data.IntMap (IntMap,(!))
import qualified Data.IntMap as IntMap

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.List ((\\))
import qualified Data.List as List

import Data.Sequence (Seq(..),ViewL(..),(><),(<|),(|>))
import qualified Data.Sequence as Seq

import Automata 
import PPerm 
import GenSystem 

data History = Small Int | Large deriving (Eq,Ord,Show)

succs :: Auto -> History -> (State, PPerm, State) -> Maybe [(State, PPerm, State, History)]
-- ^ @succs a (q1, s, q2)@ is @Just pps@ whenever @q1@ and @q2@ are one-step @s@-bisimilar
--   in @a@ with successor triples @pps@ and otherwise @Nothing@.
succs a h (q1,s,q2) =
  do q1succs <- mapM (trans id s q2) (List.filter ((==) q1 . init) (trns a))
     q2succs <- mapM (trans reflect (inverse s) q1) (List.filter ((==) q2 . init) (trns a))
     return (concat q1succs ++ concat q2succs)
  where

    r = List.length (regs a)
    swap = PPerm.swap [1..2*r]
    restrict q1 q2 = 
      rngRestrict (actv a ! q2) . domRestrict (actv a ! q1) 
    shuffleDown q1 q2 s =
      let rs1 = actv a ! q1
          rs2 = actv a ! q2
          free1 = [1..2*r] \\ rs1
          free2 = [1..2*r] \\ rs2
          triangleLeft s = List.foldr (\p pi -> swap p `compseq` pi) (idPP [1..2*r]) (List.zip s free1)
          domp = triangleLeft (dom s)
          rngp = triangleLeft (rng s)
      in  inverse domp `compseq` s `compseq` rngp
    reflect (q1, s, q2, h) = (q2, inverse s, q1, h) 
    hasInitTagFreshMode q t (q',t',m',_,_) = q == q' && t == t' && (m' == LFresh || m' == GFresh)
    hasInitTagMode q t m (q',t',m',_,_) = q == q' && t == t' && m == m'
    hasInitTagModeIndex q t m i (q',t',m',i',_) = q == q' && t == t' && m == m' && i == i'
    
    -- The conditions on successors are given, as in the draft, for q1 and then we obtain
    -- the symmetric conditions for q2 by using reflect.
    trans :: ((State, PPerm, State, History) -> (State, PPerm, State, History)) -> PPerm -> State -> Transition -> Maybe [(State, PPerm, State, History)]
    -----------------------------------------------
    -- SMALL h
    -----------------------------------------------
    ---- (a) q1 --t,i--> q1'
    trans ref s q2 (q1,t,Stored,i,q1') | Small _ <- h =
      let j = s ! i in -- guaranteed since h <= 2r
      if List.elem j (actv a ! q2) then 
        do (_,_,_,_,q2') <- List.find (hasInitTagModeIndex q2 t Stored j) (trns a)
           let s' = shuffleDown q1' q2' s
           return [ref (q1', s', q2', h)]
      else
        do (_,_,_,j',q2') <- List.find (hasInitTagMode q2 t LFresh) (trns a)
           let s' = shuffleDown q1' q2' (s `compseq` swap (j,j'))
           return [ref (q1', s', q2', h)]

    ---- (b), (c) q1 --t,i*--> q1' 
    trans ref s q2 (q1,t,LFresh,i,q1') | Small _ <- h =
      let forHistorical i' = 
            let j = s ! i' in
            if List.elem j (actv a ! q2) then
              do (_,_,_,_,q2') <- List.find (hasInitTagModeIndex q2 t Stored j) (trns a)
                 let s' = shuffleDown q1' q2' (swap (i,i') `compseq` s)
                 return (ref (q1', s', q2', h))
            else 
              do (_,_,_,j',q2') <- List.find (hasInitTagMode q2 t LFresh) (trns a)
                 let s' = shuffleDown q1' q2' (swap (i,i') `compseq` s `compseq` swap (j,j'))
                 return (ref (q1', s', q2', h))
      in do gFreshSuccs <- trans ref s q2 (q1,t,GFresh,i,q1')
            let [gFreshSucc] = gFreshSuccs -- there is only one, actually
            historicalSuccs <- 
              let historicalForQ1 = dom s \\ (actv a ! q1)
              in  mapM forHistorical historicalForQ1
            return (gFreshSucc : historicalSuccs)
            
    ---- (b), (c) q1 --t,i**--> q1'
    trans ref s q2 (q1,t,GFresh,i,q1') | Small hsz <- h =
      do (_,_,l',j,q2') <- List.find (hasInitTagFreshMode q2 t) (trns a)
         if hsz < 2*r then 
           let s' = shuffleDown q1' q2' (swap (i,2*r) `compseq` remap (2*r) (2*r) s `compseq` swap (j,2*r))
           in  return [ref (q1', s', q2', Small (hsz+1))]
         else
           let s' = restrict q1' q2' (remap i j s)
           in  return [ref (q1', s', q2', Large)]
          

    --------------------------------------------------------
    -- LARGE h
    --------------------------------------------------------
    ---- (d) q1 --t,i--> q1' 
    trans ref s q2 (q1,t,Stored,i,q1') | Large <- h =
      case IntMap.lookup i s of
      Just j -> 
        do  (_,_,_,_,q2') <- List.find (hasInitTagModeIndex q2 t Stored j) (trns a)
            let s' = restrict q1' q2' s
            return [ref (q1', s', q2', h)]
      Nothing ->
        do  (_,_,_,j,q2') <- List.find (hasInitTagMode q2 t LFresh) (trns a)
            let s' = restrict q1' q2' (remap i j s)
            return [ref (q1', s', q2', h)]

    ---- (e), (f) q1 --t,i*--> q1'
    trans ref s q2 (q1,t,LFresh,i,q1') | Large <- h =
      do (_,_,_,j,q2') <- List.find (hasInitTagMode q2 t LFresh) (trns a)
         let notRng = actv a ! q2 \\ rng s
         let find k = List.find (hasInitTagModeIndex q2 t Stored k) (trns a)
         let mkSucc k =
               do (_,_,_,_,q2') <- find k
                  return (ref (q1', remap i k s, q2', h))
         succs <- mapM mkSucc notRng
         let s' = restrict q1' q2' (remap i j s)
         return (ref (q1', s', q2', h) : succs)

    ---- (f) q1 --t,i**--> q1'
    trans ref s q2 (q1,t,GFresh,i,q1') | Large <- h =
      do (_,_,_,j,q2') <- List.find (hasInitTagFreshMode q2 t) (trns a)
         let s' = restrict q1' q2' (remap i j s)
         return [(ref (q1, s', q2', h))]


raBisim :: Auto -> (State, PPerm, State) -> Bool
-- ^ @raBisim a regs pp@ is true just if every pair of configurations 
--   represented by @pp@ are bisimilar in RA @a@.
raBisim = bisimWithoutHistory

fraBisim :: Auto -> (State, PPerm, State) -> Bool
-- ^ @fraBisim a regs pp@ is true just if every pair of configurations 
--   represented by @pp@ are bisimilar in FRA @a@.
fraBisim = bisimWithHistory

bisimWithHistory :: Auto -> (State, PPerm, State) -> Bool
bisimWithHistory a (q1,s,q2) =
  bisimWithGivenHistory a (Seq.singleton (q1,s',q2)) (Small 0)
  where
    r = List.length (regs a)
    mu = actv a
    numNotIn1 = mu ! q1 \\ dom s
    numNotIn2 = mu ! q2 \\ rng s
    freeDomSlots = [1..2*r] \\ mu ! q1
    freeRngSlots = [1..2*r] \\ mu ! q2
    extendPPerm []     ys     s = s
    extendPPerm (x:xs) (y:ys) s = extendPPerm xs ys (IntMap.insert x y s)
    s' = extendPPerm numNotIn1 freeRngSlots (extendPPerm numNotIn2 freeDomSlots s)
      
    
bisimWithoutHistory :: Auto -> (State, PPerm, State) -> Bool
bisimWithoutHistory a pp =
  bisimWithGivenHistory a (Seq.singleton pp) Large 

bisimWithGivenHistory :: Auto -> Seq (State, PPerm, State) -> History -> Bool
bisimWithGivenHistory a que h =
  case loop (initGs a) que Seq.empty of
    Nothing -> False
    Just que' ->
      case h of
        Small sz | sz < 2*r -> bisimWithGivenHistory a que' (Small (sz+1)) 
        Small _             -> bisimWithGivenHistory a que' Large          
        Large               -> True 

  where
    
    r = List.length (regs a)

    initGs :: Auto -> GenSystem
    initGs a = GenSys {
        rep = IntMap.fromList (List.map (\q -> (q, q)) $ stts a),
        chr = IntMap.fromList (List.map (\q -> (q, actv a ! q)) $ stts a),
        grp = IntMap.fromList (List.map (\q -> (q, [])) $ stts a),
        ray = IntMap.fromList (List.map (\q -> (q, toPPerm (regs a) 1)) $ stts a)
      }
  

    loop :: GenSystem -> Seq (State, PPerm, State) -> Seq (State, PPerm, State) -> Maybe (Seq (State, PPerm, State))
    loop gs quAtH quAtSuccH =
      case Seq.viewl quAtH of
        EmptyL           -> Just quAtSuccH
        (q1,s,q2) :< que ->
          if isMember gs (q1,s,q2) then
            loop gs que quAtSuccH
          else
            do pps <- succs a h (q1,s,q2)
               let (quAtH',quAtSuccH') = queueUp pps (que, quAtSuccH)
               let gs' = extend (q1,s,q2) gs
               loop gs' quAtH' quAtSuccH'

    queueUp pps (que1,que2) =
      let separateByHist (q1,s,q2,h') (que1,que2) =
            if h == h' then (que1 |> (q1,s,q2), que2) else (que1, que2 |> (q1,s,q2))
      in  List.foldr separateByHist (que1, que2) pps
