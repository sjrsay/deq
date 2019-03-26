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

import Math.Algebra.Group.PermutationGroup ((-^))              -- permutation action on lists
import Math.Algebra.Group.PermutationGroup as Permutation (p)  -- permutation from disjoint cycles constructor

import Automata 
import PPerm 
import GenSystem 

data History = Small Int | Large deriving (Eq,Ord,Show)

triangleLeft :: Int -> [Reg] -> [Reg] -> Perm
triangleLeft r s t =
  List.foldr (\(i,j) pi -> if i==j then pi else pi * Permutation.p [[i,j]]) 1 (List.zip ((t \\ [1..n]) \\ s) ([n+1..2*r] \\ s))
  where
    n = List.foldl (\j i -> if i == j + 1 then i else j) 0 (List.sort t)

invTriangleLeft :: Int -> [Reg] -> [Reg] -> Perm
invTriangleLeft r s t =
  List.foldr (\(i,j) pi -> if i==j then pi else Permutation.p [[i,j]] * pi) 1 (List.zip ((t \\ [1..n]) \\ s) ([n+1..2*r] \\ s))
  where
    n = List.foldl (\j i -> if i == j + 1 then i else j) 0 (List.sort t)

succs :: Auto -> History -> (State, PPerm, State) -> Maybe [(State, PPerm, State, History)]
-- ^ @succs a (q1, s, q2)@ is @Just pps@ whenever @q1@ and @q2@ are one-step @s@-bisimilar
--   in @a@ with successor triples @pps@ and otherwise @Nothing@.
succs a h (q1,s,q2) =
  do q1succs <- mapM (trans id s q2 q2trans) q1trans
     q2succs <- mapM (trans reflect (inverse s) q1 q1trans) q2trans
     return (concat q1succs ++ concat q2succs)
  where
    q1trans = List.filter ((==) q1 . init) (trns a)
    q2trans = List.filter ((==) q2 . init) (trns a)
    r = List.length (regs a)
    swap = PPerm.swap [1..2*r]
    restrict q1 q2 = 
      rngRestrict (actv a ! q2) . domRestrict (actv a ! q1) 
    shuffleDown q1 q2 s =
      let domp = toPPerm [1..2*r] (invTriangleLeft r (actv a ! q1) (dom s))
          rngp = toPPerm [1..2*r] (triangleLeft r (actv a ! q2) (rng s)) 
      in  domp `compseq` s `compseq` rngp
    reflect (q1, s, q2, h) = (q2, inverse s, q1, h) 
    hasTagFreshMode t (_,t',m',_,_) = t == t' && (m' == LFresh || m' == GFresh)
    hasTagMode t m (_,t',m',_,_) = t == t' && m == m'
    hasTagModeIndex t m i (_,t',m',i',_) = t == t' && m == m' && i == i'
    
    -- The conditions on successors are given, as in the draft, for q1 and then we obtain
    -- the symmetric conditions for q2 by using reflect.
    trans :: ((State, PPerm, State, History) -> (State, PPerm, State, History)) -> PPerm -> State -> [Transition] -> Transition -> Maybe [(State, PPerm, State, History)]
    --
    -- Given a transition @t = (q1, ..., q1')@, @trans ref s q2 q2trans t@ is either the list of potential successors of @q2@ after performing any
    -- transition from q2trns that is consistent with an instance of @t@, according to the current matching @s@, or is @Nothing@ just in case the bisimulation
    -- condition was violated (no matching transition available).
    --
    -- Note that in case of t being a local read of register j there is at most one matching transition because the letter is determined as contents of j
    -- but in case of t being locally fresh, the transition from q1 may be choose either a letter from inside or outside the range of s (which must be matched
    -- by a local read or locally fresh transition respectively, both of which may be available to q2).
    --
    -----------------------------------------------
    -- SMALL h
    -----------------------------------------------
    ---- (a) q1 --t,i--> q1'
    trans ref s q2 q2trns (q1,t,Stored,i,q1') | Small _ <- h =
      let j = s ! i in -- guaranteed since h <= 2r
      if List.elem j (actv a ! q2) then 
        do (_,_,_,_,q2') <- List.find (hasTagModeIndex t Stored j) q2trns
           let s' = shuffleDown q1' q2' s
           return [ref (q1', s', q2', h)]
      else
        do (_,_,_,j',q2') <- List.find (hasTagMode t LFresh) q2trns
           let s' = shuffleDown q1' q2' (s `compseq` swap (j,j'))
           return [ref (q1', s', q2', h)]

    ---- (b), (c) q1 --t,i*--> q1' 
    trans ref s q2 q2trns (q1,t,LFresh,i,q1') | Small _ <- h =
      let forHistorical i' = 
            let j = s ! i' in
            if List.elem j (actv a ! q2) then
              do (_,_,_,_,q2') <- List.find (hasTagModeIndex t Stored j) q2trns
                 let s' = shuffleDown q1' q2' (swap (i,i') `compseq` s)
                 return (ref (q1', s', q2', h))
            else 
              do (_,_,_,j',q2') <- List.find (hasTagMode t LFresh) q2trns
                 let s' = shuffleDown q1' q2' (swap (i,i') `compseq` s `compseq` swap (j,j'))
                 return (ref (q1', s', q2', h))
      -- in case of a locally fresh transition, we do everything above from part (b) and, additionally,
      -- everything from part (c) which we would do as if we had a globally fresh transition,
      -- so let's pretend it is globally fresh and do part (c)
      in do gFreshSuccs <- trans ref s q2 q2trns (q1,t,GFresh,i,q1')
            let [gFreshSucc] = gFreshSuccs -- part (c) always yields exactly one successor, if it succeeds
            historicalSuccs <- 
              -- now actually execute the part (b) conditions
              let historicalForQ1 = dom s \\ (actv a ! q1)
              in  mapM forHistorical historicalForQ1
            return (gFreshSucc : historicalSuccs)
            
    ---- (b), (c) q1 --t,i**--> q1'
    trans ref s q2 q2trns (q1,t,GFresh,i,q1') | Small hsz <- h =
      do (_,_,l',j,q2') <- List.find (hasTagFreshMode t) q2trns
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
    trans ref s q2 q2trns (q1,t,Stored,i,q1') | Large <- h =
      case IntMap.lookup i s of
      Just j -> 
        do  (_,_,_,_,q2') <- List.find (hasTagModeIndex t Stored j) q2trns
            let s' = restrict q1' q2' s
            return [ref (q1', s', q2', h)]
      Nothing ->
        do  (_,_,_,j,q2') <- List.find (hasTagMode t LFresh) q2trns
            let s' = restrict q1' q2' (remap i j s)
            return [ref (q1', s', q2', h)]

    ---- (e), (f) q1 --t,i*--> q1'
    trans ref s q2 q2trns (q1,t,LFresh,i,q1') | Large <- h =
      do (_,_,_,j,q2') <- List.find (hasTagMode t LFresh) q2trns
         let rng = IntMap.foldr (\x xs -> IntSet.insert x xs) IntSet.empty s
         let notRng = List.filter (\x -> not $ IntSet.member x rng) (actv a ! q2)
         -- Make a map of states reached by taking stored register transitions to make
         -- the following O(r) number of lookups more efficient, i.e. O(log|tags|) each
         let tk = List.foldr (\(_,t',m,k,q2') b -> if m == Stored && t' == t then IntMap.insert k q2' b else b) IntMap.empty q2trns
         let mkSucc k =
               do q2' <- IntMap.lookup k tk
                  return (ref (q1', remap i k s, q2', h))
         succs <- mapM mkSucc notRng
         let s' = restrict q1' q2' (remap i j s)
         return (ref (q1', s', q2', h) : succs)

    ---- (f) q1 --t,i**--> q1'
    trans ref s q2 q2trns (q1,t,GFresh,i,q1') | Large <- h =
      do (_,_,_,j,q2') <- List.find (hasTagFreshMode t) q2trns
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
  bisimWithGivenHistory a (Seq.singleton (q1,s',q2)) (Small (IntMap.size s'))
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
      -- Note: it can be that h != Large and yet que' is empty, in which case
      -- the loop will iterate trivially (bisimWithGivenHistory will just return
      -- immediately) until h = Large.
      case h of
        Small sz | sz < 2*r -> bisimWithGivenHistory a que' (Small (sz+1)) 
        Small _             -> bisimWithGivenHistory a que' Large          
        Large               -> True

  where
    
    r = List.length (regs a)

    initGs :: Auto -> GenSystem
    initGs a = GenSys {
        rep = IntMap.fromList (List.map (\q -> (q, q)) $ stts a),
        chr = IntMap.fromList (List.map (\q -> (q, initRegs q)) $ stts a),
        grp = IntMap.fromList (List.map (\q -> (q, [])) $ stts a),
        ray = IntMap.fromList (List.map (\q -> (q, toPPerm (initRegs q) 1)) $ stts a)
      }
      where
        initRegs q =
          let muq = actv a ! q in
          case h of 
            Small sz -> 
              let t = muq ++ [r+1..sz-List.length muq]
              in  List.sort (t -^ triangleLeft r muq t)
            Large    -> muq
  

    loop :: GenSystem -> Seq (State, PPerm, State) -> Seq (State, PPerm, State) -> Maybe (Seq (State, PPerm, State))
    -- ^ @loop gs quAtH quAtSuccH@ is either @Nothing@ if one of the tuples in @quAtH@ causes the successor computation
    -- to fail (because it violates the synbolic bisimulation conditions) or @Just quAtSuccH'@ where @quAtSuccH'@ is
    -- the collection of tuples at the next level of histor @quAtSuccH@ but with additional tuples added that were generated 
    -- by considering the successors of the tuples @quAtH@ at the current level.
    --
    -- Note: the generating system @gs@ is updated as @quAtH@ is processed, but ultimately not returned.
    loop gs quAtH quAtSuccH =
      -- process the queue of tuples associated with the current history h
      case Seq.viewl quAtH of
        -- Once the current level of history is empty, we just return the accumulated next level of history
        EmptyL           -> Just quAtSuccH
        -- Otherwise we process the head of the queue and recurse on the tail
        (q1,s,q2) :< que ->
          if isMember gs (q1,s,q2) then
            loop gs que quAtSuccH
          else
            -- inside the Maybe monad: propogate a Nothing coming from succs
            do pps <- succs a h (q1,s,q2)
               let (quAtH',quAtSuccH') = queueUp pps (que, quAtSuccH)
               let gs' = extend (q1,s,q2) gs
               loop gs' quAtH' quAtSuccH'

    -- Any new tuple produced by the successor computation at history h is guaranteed to be at level h or the next level
    -- queue up seperates tuples associted with the two histories and queues them into the given que1 or que2 respectively.
    queueUp pps (que1,que2) =
      let separateByHist (q1,s,q2,h') (que1,que2) =
            if h == h' then (que1 |> (q1,s,q2), que2) else (que1, que2 |> (q1,s,q2))
      in  List.foldr separateByHist (que1, que2) (pps)
