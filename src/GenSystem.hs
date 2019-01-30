module GenSystem where

import qualified Data.List as List

import Data.IntMap (IntMap,(!))
import qualified Data.IntMap as IntMap

import Automata (Reg,State,StateMap)
import PPerm 

--
-- | Generating systems as a representation for bisimulations over register automata.
--
--
data GenSystem =
  GenSys {
    rep :: StateMap State,
      -- ^ If @rep q@ is the representative of the equivalence class containing @q@.
    chr :: StateMap [Reg],
      -- ^ If @q@ is in the range of @rep@ then @chr q@ is the characteristic set of @q@.
    grp :: StateMap [Perm],
      -- ^ If @q@ is in the range of @rep@ then @grp q@ is the generators of the 
      --   permutation group over @chr q@ that is associated with @q@.  Generators must have
      --   non-trivial support so, in partiular, the identity is never among them.
    ray :: StateMap PPerm
      -- ^ If @q@ is in the domain of @rep@ but not in its range then @ray q@ is the partial
      --   permutation witnessing membership of @q@.
  }
  deriving (Eq,Show)

equiv :: GenSystem -> State -> State -> Bool
-- ^ @equiv gs q1 q2@ is true just if @q1@ and @q2@ are equivalent in @gs@
equiv gs q1 q2 =
  rep gs ! q1 == rep gs ! q2

domain :: GenSystem -> [State]
-- ^ @domain gs@ is the set of states in @gs@
domain gs = IntMap.keys (rep gs)

representatives :: GenSystem -> [State]
-- ^ @representatives gs@ is the set of states that act as representatives in @gs@
representatives gs = List.nub (IntMap.elems (rep gs))

nonRepresentatives :: GenSystem -> [State]
-- ^ @nonRepresentatives gs@ is the set of states that are in @gs@ but are not
--   representatives of equivalence classes
nonRepresentatives gs = IntMap.keys (ray gs)

-- partialGens :: GenSystem -> [(State, PPerm, State)]
-- ^ @partialGens gs@ is the set of partial permutations that comprises the generating
-- system, the "canonical subset" of the draft.  It is /not/ the set of partial 
-- permutations so generated (the closure).
-- partialGens gs =
--   [ (q, permToPPerm q pi, q) | q <- representatives gs, pi <- Permutation.elts $ grp gs ! q ]
--     ++ [ (rep gs ! q, ray gs ! q, q) | q <- nonRepresentatives gs ]
--   where permToPPerm q = toPPerm (chr gs ! q )
  
isMember :: GenSystem -> (State, PPerm, State) -> Bool
-- ^ @isMember gs (q1, s, q2)@ is true just if @(q1, s, q2)@ is in the closure of @gs@.
isMember gs (q1, s, q2) =
  q1 `eqv` q2
    && dom s' == x -- assumes x is ordered canonically
    && isMem (grp gs ! q) (toPerm s')
  where
    q = rep gs ! q1
    x = chr gs ! q
    s' = (ray gs ! q1) `compseq` s `compseq` inverse (ray gs ! q2)
    eqv = equiv gs
    
extend :: (State, PPerm, State) -> GenSystem -> GenSystem
-- ^ @extend (q1, s, q2) gs@ is a generating system whose closure includes the closure of
-- @gs@ and also @(q1, s, q2)@.
extend (q1,s,q2) gs | equiv gs q1 q2 =
  if dom s' == x then
    -- add s' to generators maintaining sortedness
    let sgs = mksgs (p:g)
    in  gs { grp = IntMap.insert q sgs (grp gs) }
  else
    -- adjust x
    let ss = s' : List.map (toPPerm x) g 
        x' = List.sort (ch x ss) 
        g' = mksgs (List.map (toPerm . domRestrict x') ss)
        ray' = IntMap.foldWithKey (rayUpdate x') IntMap.empty (ray gs) 
    in  gs { chr = IntMap.insert q x' (chr gs), grp = IntMap.insert q g' (grp gs), ray = ray' }
  where
    q  = rep gs ! q1
    g  = grp gs ! q
    x  = chr gs ! q
    s' = (ray gs ! q1) `compseq` s `compseq` inverse (ray gs ! q2)
    p  = toPerm s'

    rayUpdate x q' rayq ray' =
      if equiv gs q' q then
        -- q' is also represented by q
        IntMap.insert q' (domRestrict x rayq) ray'
      else
        IntMap.insert q' rayq ray'

extend (q1,s,q2) gs | not (equiv gs q1 q2) =
  let rep' = IntMap.map (\q -> if q == q1' || q == q2' then q1' else q) (rep gs) in
  let h    = List.map (toPPerm x1) g1 ++ List.map (\ge -> s1 `compseq` toPPerm x2 ge `compseq` s2) g2 in
  let x    = ch x1 h in
  let g'   = mksgs (List.map (toPerm . domRestrict x) h) in
  let grp' = IntMap.insert q1' g' (IntMap.delete q2' (grp gs)) in
  let chr' = IntMap.insert q1' x (IntMap.delete q2' (chr gs)) in
  let ray' = IntMap.foldWithKey (rayUpdate x) IntMap.empty (ray gs) in
  GenSys { rep = rep', chr = chr', grp = grp', ray = ray' }
  where
    q1' = rep gs ! q1
    q2' = rep gs ! q2
    x1  = chr gs ! q1'
    x2  = chr gs ! q2'
    g1  = grp gs ! q1'
    g2  = grp gs ! q2'
    s1  = (ray gs ! q1) `compseq`         s `compseq` inverse (ray gs ! q2)
    s2  = (ray gs ! q2) `compseq` inverse s `compseq` inverse (ray gs ! q1)

    rayUpdate x q rayq ray' =
      let q' = rep gs ! q in
      if q' == q1' then
        IntMap.insert q (domRestrict x rayq) ray'
      else if q' == q2' then
        let ray'q =
             (ray gs ! q1) `compseq` s `compseq` inverse (ray gs ! q2) `compseq` rayq
        in IntMap.insert q (domRestrict x ray'q) ray'
      else
        IntMap.insert q rayq ray'

extendAll :: [(State, PPerm, State)] -> GenSystem -> GenSystem
-- ^ @extendAll pps gs@ is a generating system whose closure includes the closure of
-- @gs@ and also all of @pps@.
extendAll = flip (List.foldr extend)