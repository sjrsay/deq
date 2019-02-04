{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

  import Prelude hiding (readFile, writeFile)
  import Data.Data (Data)
  import Data.Typeable (Typeable)
  import Data.IntMap (IntMap)
  import qualified Data.IntMap as IntMap
  import qualified Data.List as List
  import Data.Bimap (Bimap)
  import qualified Data.Bimap as Bimap
  import qualified Control.Monad as Monad
  import qualified Data.Either as Either
  import System.Console.CmdArgs.Implicit ((&=),summary,cmdArgs,help,program,argPos)
  import Text.XML
  import Text.XML.Cursor
  import System.Exit
  import Data.Text (Text)
  import qualified Data.Text as Text
  import Data.Text.Read (decimal)
  import qualified System.CPUTime as CPUTime
  import Text.Printf (printf)
  import qualified Data.Maybe as Maybe

  import Automata
  import qualified Bisim

  type SynAutomaton = (Text, [(Text, [Text])], Text, [(Text, Text, Text, Text, Text)])

  data Args =
    Args {
      auto1 :: String, 
      auto2 :: String
    }
    deriving (Show,Data,Typeable)
  
  getElementVal :: Cursor -> Name -> Text
  getElementVal c n = head $ c $/ element n &/ content

  argSpec = 
    Args { 
      auto1 = "" &= argPos 0,
      auto2 = "" &= argPos 1
    } &= summary "DRAEquiv 1.1 : Equivalence testing for deterministic register automata."
      &= help "Where each ITEM is the filepath of an xml spcification of a D(F)RA."
      &= program "deq"

  pState :: Cursor -> (Text, [Text])
  pState c =
      (q, muq)
    where
      q = getElementVal c "id"
      muq = c $/ element "available-registers" &/ element "register" &/ content

  pTransition :: Cursor -> (Text, Text, Text, Text, Text)
  pTransition c =
      (q1, tag, mode, reg, q2)
    where
      q1 = getElementVal c "from"
      tag = getElementVal c "input"
      mode = getElementVal c "op"
      reg = getElementVal c "register"
      q2 = getElementVal c "to"
  
  parseAuto :: Cursor -> SynAutomaton
  parseAuto c =
      (regs, states, init, transitions)
    where
      regs = getElementVal c "registers"
      init = getElementVal c "initial-state"
      states = c $/ element "states" &/ element "state" &| pState
      transitions = c $/ element "transitions" &/ element "transition" &| pTransition
      
  mkAutomaton :: Bimap Text Tag -> SynAutomaton -> Either String (Auto, State)
  mkAutomaton tDictI (rtxt, qstxt, q0txt, tstxt) =
    do  q0 <- q0Parse
        muq <- Monad.foldM insertActv IntMap.empty qstxt
        ts  <- mapM mkTransition tstxt
        let r = List.foldr (\(_,_,_,r,_) m -> max r m) 0 ts
        let a = 
             Auto { 
                regs = [1..r],
                stts = Bimap.keysR qDict,
                actv = muq,
                trns = ts,
                tagn = tDict,
                sttn = qDict
              }
        -- Check that the initial state has no active registers
        if (Maybe.fromJust $ IntMap.lookup q0 muq) /= [] then 
          Left "Initial state must have no active registers." 
        else Right ()
        -- Check that transitions respect active registers
        Monad.mapM (transConsistent muq) ts
        -- Check that the transitions are deterministic
        case deterministic ts of
          []  -> Right ()
          g:_ -> 
            let q1 = qDict Bimap.!> Automata.init (g !! 1) in
            let t  = tDict Bimap.!> Automata.tag (g !! 1)  in
            Left ("Transition relation is not deterministic, for example, there can be more than 1 applicable transition from state " ++ show q1 ++ " on input " ++ show t ++ ".")
        return (a, q0)
    
    where
      transConsistent muq (q1,_,Stored,i,q2) =
        let muq1 = Maybe.fromJust (IntMap.lookup q1 muq) in
        let muq2 = Maybe.fromJust (IntMap.lookup q2 muq) in
        if not (List.elem i muq1) then 
          Left ("Inconsistent transition: " ++ show (qDict Bimap.!> q1) ++ "does not have " ++ show i ++ " active.")
        else if not (List.isSubsequenceOf muq2 muq1) then
          Left ("Inconsistent transition: on a read transition, every active register of " ++ show (qDict Bimap.!> q2) ++ " must be contained in the active registers of " ++ show (qDict Bimap.!> q1) ++ ".")
        else
          Right ()
      
      transConsistent muq (q1,_,_,i,q2) =
        let muq1 = Maybe.fromJust (IntMap.lookup q1 muq) in
        let muq2 = Maybe.fromJust (IntMap.lookup q2 muq) in
        if not (List.isSubsequenceOf (List.delete i muq2) muq1) then
          Left ("Inconsistent transition: on a fresh" ++ show i ++ "transition from " ++ show (qDict Bimap.!> q1) ++ " to " ++ show (qDict Bimap.!> q2) ++ ", every active register of " ++ show (qDict Bimap.!> q2) ++ " must be either index " ++ show i ++ " or contained in the active registers of " ++ show (qDict Bimap.!> q1) ++ ".")
        else
          Right ()

      deterministic ts =
        List.filter (\xs -> List.length xs /= 1) (List.groupBy eq ts)
        where
          eq (q1,tag,Stored,r,_) (q1',tag',Stored,r',_) = q1 == q1' && tag == tag' && r == r'
          eq (q1,tag,LFresh,_,_) (q1',tag',LFresh,_,_)  = q1 == q1' && tag == tag'
          eq (q1,tag,LFresh,_,_) (q1',tag',GFresh,_,_)  = q1 == q1' && tag == tag'
          eq (q1,tag,GFresh,_,_) (q1',tag',GFresh,_,_)  = q1 == q1' && tag == tag'
          eq _ _                                        = False


      parseNum :: Text -> Maybe Int
      parseNum d = either (\_ -> Nothing) (\(i,_) -> Just i) (decimal d)

      q0Parse =
        case Bimap.lookup q0txt qDict of
          Just q0 -> Right q0
          Nothing -> Left ("Error parsing initial state: " ++ show q0txt ++ " was not understood from <states>...</states>.")

      insertActv :: IntMap [Reg] -> (Text, [Text]) -> Either String (IntMap [Reg])
      insertActv m (qtxt,rstxt) =
          case mbMu of
            Just mu -> Right mu
            Nothing -> Left ("Error parsing state: " ++ show qtxt ++ " had an invalid available register number.")
        where
          mbMu =
            do  i <- Bimap.lookup qtxt qDict 
                rs <- mapM parseNum rstxt
                return (IntMap.insert i (List.sort rs) m)
    
      mkTransition :: (Text,Text,Text,Text,Text) -> Either String (State,Tag,Mode,Reg,State)
      mkTransition (tt@(q1txt,ttxt,mtxt,rtxt,q2txt)) =
        do  q1 <- case Bimap.lookup q1txt qDict of
                      Just q1 -> Right q1
                      Nothing -> Left ("Error parsing transition: " ++ show tt ++ ", the source state was not understood from <states>...</states>.")
            t  <- Right (Maybe.fromJust (Bimap.lookup ttxt tDict)) -- This can't fail
            m  <- case mtxt of
                    "Read"   -> Right Stored
                    "LFresh" -> Right LFresh
                    "GFresh" -> Right GFresh
                    _        -> Left ("Error parsing transition: " ++ show tt ++ ", " ++ show mtxt ++ " is not one of \"read\", \"lfresh\" or \"gfresh\".")
            r <- case parseNum rtxt of 
                    Just r | r <= 0 -> Left ("Error parsing transition: " ++ show tt ++ ", register index must be at least 1.")
                    Just r -> Right r
                    Nothing -> Left ("Error parsing transition: " ++ show tt ++ ", " ++ show rtxt ++ " is not a valid register index.")
            q2 <- case Bimap.lookup q2txt qDict of
                    Just q2 -> Right q2
                    Nothing -> Left ("Error parsing transition: " ++ show tt ++ ", the target state was not understood from <states>...</states>.")
            return (q1,t,m,r,q2)
    
      mkBij d = snd . List.foldr insertNumbered (Bimap.size d, d)
      qDict = mkBij Bimap.empty (List.map fst qstxt)
      tDict = mkBij tDictI (List.map (\(_,t,_,_,_) -> t) tstxt)
      insertNumbered :: Text -> (Int, Bimap Text Int) -> (Int, Bimap Text Int)
      insertNumbered k (i, dict) =
          if Bimap.member k dict then (i, dict) else (i+1, dict')
        where
          dict' = Bimap.insert k i dict
      

  main :: IO ()
  main =
    do  args <- cmdArgs argSpec
        doc1 <- readFile def (auto1 args)
        doc2 <- readFile def (auto2 args)
        let cursor1 = fromDocument doc1
        let cursor2 = fromDocument doc2
        (a1, q01) <- 
          case mkAutomaton Bimap.empty (parseAuto cursor1) of
            Left e -> 
              do  putStrLn ("[Automaton 1] " ++ e)
                  exitWith (ExitFailure 1)
            Right x -> return x
        (a2, q02) <-
          case mkAutomaton (tagn a1) (parseAuto cursor2) of
            Left e -> 
              do  putStrLn ("[Automaton 2] "++ e)
                  exitWith (ExitFailure 1)
            Right x -> return x
        let (sumAuto, inl, inr) = Automata.sum a1 a2
        let reqGFresh = Automata.reqGlobalFreshness sumAuto
        start <- CPUTime.getCPUTime
        let b = (if reqGFresh then Bisim.fraBisim else Bisim.raBisim) sumAuto (inl q01, IntMap.fromList [], inr q02) 
        putStr $ auto1 args ++ ", " ++ auto2 args ++ ", " ++ show b ++ ", "
        stop <- CPUTime.getCPUTime
        let diff = (fromIntegral (stop - start)) / (10^9)
        printf "%.0f\n" (diff :: Double)