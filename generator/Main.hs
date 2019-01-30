module Main where

import Text.XML.HXT.Core 
import qualified System.Random as Random
import qualified System.Exit as Exit
import qualified System.Environment as Environment
import qualified Data.List as List

data Mode = Stored | LFresh | GFresh
  deriving (Show,Read)

data Dir = LR | RL

main :: IO [XmlTree]
main =
  do  [mode, sz] <- Environment.getArgs
      let n = read sz
      xmlRoot <-
        case mode of
          "lrstack" -> return (mkStack LR n)
          "rlstack" -> return (mkStack RL n)
          "lrstack-alt" -> return (mkAltStack LR n)
          "rlstack-alt" -> return (mkAltStack RL n)
          "lrralib" -> return (mkRALibStack LR n)
          "rlralib" -> return (mkRALibStack RL n)
          "gloloL" -> return (mkGloLo LFresh n)
          "gloloG" -> return (mkGloLo GFresh n)
          "cpt"     -> 
            do  p <- Random.randomRIO (0, List.product [1..n] - 1)
                return (mkCpt p n)
          _         -> 
            do  putStrLn "The first argument must be one of \"lrstack\", ..."
                Exit.exitFailure 
      let rootNodes = 
            if mode == "lrralib" || mode == "rlralib" then
              [xmlRoot]
            else 
              [mkDTDDoctype [("name", "dra"), ("SYSTEM", "dra.dtd")] none, xmlRoot]
      runX $ 
        root [] rootNodes
        >>>
        writeDocument [withIndent yes] (mode ++ "-" ++ sz ++ ".xml")

mkAltStack :: ArrowXml a => Dir -> Int -> a XmlTree XmlTree
-- Crete the XML spec for an alternating LR or RL stack DRA for use in the RALib comparison.
mkAltStack dir sz =
  selem "dra" [
    selem "states" [ state q qaMap | q <- [1..3*(sz+1)] ],
      selem "initial-state" [ txt "q1" ],
      selem "transitions" $ 
        pushes ++ iskips ++ pops ++ oskips ++ fakepop ++ fakepush
  ]
  
  where
    
    pushes = [ pushTransition q | q <- [1..3*sz], q `mod` 3 == 1 ] -- last push is fake
    iskips = [ skipITransition q | q <- [4..3*(sz+1)], q `mod` 3 == 1 ] 
    pops   = [ popTransition q | q <- [4..3*(sz+1)], q `mod` 3 == 2 ] -- there is no pop initially
    oskips = [ skipOTransition q | q <- [1..3*sz], q `mod` 3 == 0 ]
    fakepop = [
        selem "transition" [
          selem "from" [ txt "q1" ],
          selem "input" [ txt "ISkip" ],
          selem "op" [ txt "LFresh" ],
          selem "register" [ txt (show 1) ],
          selem "to" [ txt "q2" ]
        ],
        selem "transition" [
          selem "from" [ txt "q2" ],
          selem "input" [ txt "OSkip" ],
          selem "op" [ txt "Read" ],
          selem "register" [ txt (show 1) ],
          selem "to" [ txt "q1" ]
        ]
      ]
    fakepush = [
        selem "transition" [
          selem "from" [ txt $ "q" ++ (show $ 3*(sz+1) - 2) ],
          selem "input" [ txt "IPush" ],
          selem "op" [ txt "LFresh" ],
          selem "register" [ txt (show 1) ],
          selem "to" [ txt $ "q" ++ (show $ 3*(sz+1)) ]
        ],
        selem "transition" [
          selem "from" [ txt $ "q" ++ (show $ 3*(sz+1)) ],
          selem "input" [ txt "OSkip" ],
          selem "op" [ txt "Read" ],
          selem "register" [ txt (show 1) ],
          selem "to" [ txt $ "q" ++ (show $ 3*(sz+1) - 2) ]
        ]
      ]

    regs =
      case dir of
        LR -> [2..sz+1]
        RL -> (List.reverse [2..sz+1])

    qaMap q =
      if q `mod` 3 == 2 || q == 3*(sz+1) then 1 : rest else rest
      where
        q' = q `div` 3
        rest = List.take q' regs

    register r =
      selem "register" [ txt (show r) ]

    pushTransition i =
      selem "transition" [
        selem "from" [ txt ("q" ++ show i) ],
        selem "input" [ txt "IPush" ],
        selem "op" [ txt "LFresh" ],
        selem "register" [ txt (show $ regs !! (i `div` 3)) ],
        selem "to" [ txt ("q" ++ show (i+2)) ]
      ]

    popTransition i =
      selem "transition" [
        selem "from" [ txt ("q" ++ show i) ],
        selem "input" [ txt "OPop" ],
        selem "op" [ txt "Read" ],
        selem "register" [ txt (show $ regs !! ((i `div` 3) - 1)) ],
        selem "to" [ txt ("q" ++ show (i-4)) ]
      ]

    skipITransition i =
      selem "transition" [
        selem "from" [ txt ("q" ++ show i) ],
        selem "input" [ txt "ISkip" ],
        selem "op" [ txt "LFresh" ],
        selem "register" [ txt (show 1) ],
        selem "to" [ txt ("q" ++ show (i+1)) ]
      ]

    skipOTransition i =
      selem "transition" [
        selem "from" [ txt ("q" ++ show i) ],
        selem "input" [ txt "OSkip" ],
        selem "op" [ txt "Read" ],
        selem "register" [ txt (show $ regs !! ((i `div` 3) - 1)) ],
        selem "to" [ txt ("q" ++ show (i+1)) ]
      ]

    state q f =
      selem "state" [
        selem "id" [txt ("q" ++ show q)],
        selem "available-registers" [ register r | r <- f q ]
      ]

mkRALibStack :: ArrowXml a => Dir -> Int -> a XmlTree XmlTree
-- Crete the XML spec for an alternating LR or RL stack DRA for use in the RALib comparison.
mkRALibStack dir sz =
  selem "register-automaton" [
    selem "alphabet" [ 
      selem "inputs" [
        mkelem "symbol" [sattr "name" "ISkip"] [],
        mkelem "symbol" [sattr "name" "IPush"] [
          mkelem "param" [sattr "type" "int", sattr "name" "p0"] []
        ]
      ],
      selem "outputs" [
        mkelem "symbol" [sattr "name" "OSkip"] [],
        mkelem "symbol" [sattr "name" "OPop"] [
          mkelem "param" [sattr "type" "int", sattr "name" "p0"] []
        ]
      ]
    ],
    selem "constants" [],
    selem "globals" [ variable i | i <- [1..sz+1] ],
    selem "locations" (failed : initial : [ location q | q <- [2..3*(sz+1)] ]),
    selem "transitions" $ 
      pushes ++ fpushes ++ iskips ++ pops ++ oskips ++ fakepop ++ fakepush
  ]
  
  where
    
    pushes = [ pushTransition q | q <- [1..3*sz], q `mod` 3 == 1 ] -- last push is fake
    fpushes = [ failedTransition  q | q <- [1..3*sz], q `mod` 3 == 1 ] -- last push is fake
    iskips = [ skipITransition q | q <- [4..3*(sz+1)], q `mod` 3 == 1 ] 
    pops   = [ popTransition q | q <- [4..3*(sz+1)], q `mod` 3 == 2 ] -- there is no pop initially
    oskips = [ skipOTransition q | q <- [1..3*sz], q `mod` 3 == 0 ]
    fakepop = [
        mkelem "transition" [sattr "from" "q1", sattr "to" "q2", sattr "symbol" "ISkip"] [],
        mkelem "transition" [sattr "from" "q2", sattr "to" "q1", sattr "symbol" "OSkip"] [],
        mkelem "transition" [sattr "from" "qF", sattr "to" "q1", sattr "symbol" "OSkip"] []
      ]
    fakepush = 
      let i = 3 * (sz + 1) in
        [
          mkelem "transition" [sattr "from" ("q" ++ show (i-2)), sattr "to" ("q" ++ show i), sattr "symbol" "IPush", sattr "params" "p"]
            [
              selem "assignments" [
                mkelem "assign" [sattr "to" "r1"] [txt "p"]
              ]
            ],
          mkelem "transition" [sattr "from" ("q" ++ show i), sattr "to" ("q" ++ show (i-2)), sattr "symbol" "OSkip"] []
        ]

    regs =
      case dir of
        LR -> [2..sz+1]
        RL -> (List.reverse [2..sz+1])

    variable i = 
      mkelem "variable" [sattr "type" "int", sattr "name" ("r" ++ show i)] [txt (show i)]

    mkDiffGuard q =
        List.concat (List.intersperse " && " guards)
      where
        q' = q `div` 3
        rest = List.take q' regs
        available = 
          if q `mod` 3 == 2 || q == 3*(sz+1) then 1 : rest else rest
        guards = List.map (\i -> "p != r" ++ show i) available

    mkNotDiffGuard q =
          List.concat (List.intersperse " || " guards)
        where
          q' = q `div` 3
          rest = List.take q' regs
          available = 
            if q `mod` 3 == 2 || q == 3*(sz+1) then 1 : rest else rest
          guards = List.map (\i -> "p == r" ++ show i) available

    failedTransition i =
      mkelem "transition" [from, to, symbol, params] [
          selem "guard" [
            txt (mkNotDiffGuard i)
          ]
        ]
      where
        from = sattr "from" ("q" ++ show i)
        to   = sattr "to" ("qF")
        symbol = sattr "symbol" "IPush"
        params = sattr "params" "p"

    pushTransition i =
        mkelem "transition" [from, to, symbol, params] [
          selem "guard" [
            txt (mkDiffGuard i)
          ],
          selem "assignments" [
            mkelem "assign" [sattr "to" ("r" ++ (show $ regs !! (i `div` 3)))] [txt "p"]
          ]
        ]
      where
        from = sattr "from" ("q" ++ show i)
        to   = sattr "to" ("q" ++ show (i+2))
        symbol = sattr "symbol" "IPush"
        params = sattr "params" "p"

    popTransition i =
        mkelem "transition" [from, to, symbol, params] []
      where
        from = sattr "from" ("q" ++ show i)
        to   = sattr "to" ("q" ++ show (i-4))
        symbol = sattr "symbol" "OPop"
        params = sattr "params" ("r" ++ show (regs !! ((i `div` 3) - 1)))

    skipITransition i =
        mkelem "transition" [from, to, symbol] []
      where
        from = sattr "from" ("q" ++ show i)
        to   = sattr "to" ("q" ++ show (i+1))
        symbol = sattr "symbol" "ISkip"

    skipOTransition i =
        mkelem "transition" [from, to, symbol] []
      where
        from = sattr "from" ("q" ++ show i)
        to   = sattr "to" ("q" ++ show (i+1))
        symbol = sattr "symbol" "OSkip"

    location q = mkelem "location" [sattr "name" ("q" ++ show q)] []
    initial = mkelem "location" [sattr "name" "q1", sattr "initial" "true"] []
    failed = mkelem "location" [sattr "name" "qF"] []


mkStack :: ArrowXml a => Dir -> Int -> a XmlTree XmlTree
-- Create the XML spec for a simple LR or RL stack for use in the e.g. LOIS comparison.
mkStack dir sz =
  selem "dra" [
      selem "states" [ state q qaMap | q <- [1..sz+1] ],
      selem "initial-state" [ txt "q1" ],
      selem "transitions" $ [ pushTransition q qtMap | q <- [1..sz] ] ++ [ popTransition q qtMap | q <- [1..sz] ]
    ]
  
  where
    
    qaMap q =
      case dir of
        LR -> [1..q-1]
        RL -> List.drop (sz-q+1) [1..sz]
    
    qtMap q =
      case dir of
        LR -> q
        RL -> sz + 1 - q

    register r =
      selem "register" [ txt (show r) ]

    pushTransition i f =
      selem "transition" [
        selem "from" [ txt ("q" ++ show i) ],
        selem "input" [ txt "push" ],
        selem "op" [ txt "LFresh" ],
        selem "register" [ txt (show $ f i) ],
        selem "to" [ txt ("q" ++ show (i+1)) ]
      ]

    popTransition i f =
      selem "transition" [
        selem "from" [ txt ("q" ++ show (i+1)) ],
        selem "input" [ txt "pop" ],
        selem "op" [ txt "Read" ],
        selem "register" [ txt (show $ f i) ],
        selem "to" [ txt ("q" ++ show i) ]
      ]

    state q f =
      selem "state" [
        selem "id" [txt ("q" ++ show q)],
        selem "available-registers" [ register r | r <- f q ]
      ]

mkCpt :: ArrowXml a => Int -> Int -> a XmlTree XmlTree
-- 
mkCpt p sz =
  selem "dra" [
      selem "states" (
        end : [ state q | q <- [1..sz] ]
      ),
      selem "initial-state" [ txt "q1" ],
      selem "transitions" $ 
        [ populateFreshTransition q  | q <- [1..sz] ] 
        ++ [ populateReadTransition q j | q <- [1..sz], j <- List.take (q-1) regs ]
        ++ [ endFreshTransition i | i <- regs ]
        ++ [ endReadTransition i j | i <- regs, j <- regs ]
    ]
  
  where
    
    regs = List.permutations [1..sz] !! p

    endState = sz+1

    register r =
      selem "register" [ txt (show r) ]

    populateFreshTransition i =
      selem "transition" [
        selem "from" [ txt ("q" ++ show i) ],
        selem "input" [ txt "populate" ],
        selem "op" [ txt "LFresh" ],
        selem "register" [ txt (show $ regs !! (i - 1)) ],
        selem "to" [ txt ("q" ++ show (i+1)) ]
      ]

    populateReadTransition i j =
      selem "transition" [
        selem "from" [ txt ("q" ++ show i) ],
        selem "input" [ txt "populate" ],
        selem "op" [ txt "LFresh" ],
        selem "register" [ txt (show j) ],
        selem "to" [ txt ("q" ++ show (i+1)) ]
      ]

    endFreshTransition i =
      selem "transition" [
        selem "from" [ txt ("q" ++ show endState) ],
        selem "input" [ txt ("t" ++ show i) ],
        selem "op" [ txt "LFresh" ],
        selem "register" [ txt (show i) ],
        selem "to" [ txt ("q" ++ show endState) ]
      ]

    endReadTransition i j =
      selem "transition" [
        selem "from" [ txt ("q" ++ show endState) ],
        selem "input" [ txt ("t" ++ show i) ],
        selem "op" [ txt "Read" ],
        selem "register" [ txt (show j) ],
        selem "to" [ txt ("q" ++ show endState) ]
      ]
  
    end =
      selem "state" [
        selem "id" [txt ("q" ++ show endState)],
        selem "available-registers" [ register r | r <- regs ]
      ]

    state q =
      selem "state" [
        selem "id" [txt ("q" ++ show q)],
        selem "available-registers" [ register r | r <- List.take (q-1) regs ]
      ]

mkGloLo :: ArrowXml a => Mode -> Int -> a XmlTree XmlTree
-- 
mkGloLo mode sz =
  selem "dra" [
      selem "states" (
        end : [ state q | q <- [0..sz] ]
      ),
      selem "initial-state" [ txt "q0" ],
      selem "transitions" $ 
        endFreshTransition : [ populateFreshTransition q  | q <- [0..sz-1] ]
    ]
  
  where
    
    regs = [1..sz]

    endState = sz+1

    register r =
      selem "register" [ txt (show r) ]

    populateFreshTransition i =
      selem "transition" [
        selem "from" [ txt ("q" ++ show i) ],
        selem "input" [ txt "input" ],
        selem "op" [ txt "GFresh" ],
        selem "register" [ txt (show $ regs !! i) ],
        selem "to" [ txt ("q" ++ show (i+1)) ]
      ]

    endFreshTransition =
      selem "transition" [
        selem "from" [ txt ("q" ++ show (endState - 1)) ],
        selem "input" [ txt "input" ],
        selem "op" [ txt (show mode) ],
        selem "register" [ txt (show sz) ],
        selem "to" [ txt ("q" ++ show endState) ]
      ]
  
    end =
      selem "state" [
        selem "id" [txt ("q" ++ show endState)],
        selem "available-registers" [ register r | r <- regs ]
      ]

    state q =
      selem "state" [
        selem "id" [txt ("q" ++ show q)],
        selem "available-registers" [ register r | r <- List.take q regs ]
      ]


