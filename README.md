# deq
A Haskell library for deciding language equivalence of deterministic register automata built on the permutation groups library provided by [HaskellForMaths](https://hackage.haskell.org/package/HaskellForMaths-0.4.8).

## Getting started
You will need to have [Stack](http://www.haskellstack.org) installed to build the Haskell source.  After downloading the source, run `stack setup` and `stack run` from the root directory.  Running `stack haddock` will build the API documentation for the library.

The main type is the type of register automata `Automata.Auto`, which is a record with the following shape:
```haskell
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
```
The functions:
```haskell
  Bisim.raBisim :: Auto -> (State, PPerm, State) -> Bool
  Bisim.fraBisim :: Auto -> (State, PPerm, State) -> Bool
```
decide bisimilarity for ordinary- and fresh-register automata respectively, from a given symbolic configuration in `(State, PPerm, State)`.

## Running the experiments
To run the experiments, please follow the instructions in experiments/README.
