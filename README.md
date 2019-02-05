# deq
A tool for deciding language equivalence of deterministic register automata (DRA).

## Getting started
You will need to have [Stack](http://www.haskellstack.org) installed to build the Haskell source.  To compile, issue the following command at the root of the repository:
```
stack build
```

Within the respository, the compiled binaries can be run via stack (but this incurs an overhead):
```
  stack exec deq -- examples/lrstack-3.xml examples/rlstack-3.xml
```

Alternatively, the compiled binaries can be installed locally by running `stack install`.  The default install location is `~/.local/bin` but this can be changed using the flag `--local-bin-path`.  Then the program can be run as normal.
```
  ~/.local/bin/deq examples/lrstack-3.xml examples/rlstack-3.xml
```

In general, by invoking the tool on the command line as follows:
```
  deq /path/to/dra1.xml /path/to/dra2.xml
```
the language equivalence of the two DRA A1 and A2 specified in dra1.xml and dra2.xml will be decided.  Assuming that these two XML specifications are valid, the output of the tool has the form:
```
/path/to/dra1.xml, /path/to/dra2.xml, b, t
```
where `t` is the time taken to execute the decision procedure in milliseconds and `b` is `True` if A1 and A2 accept the same language and `b` is `False` otherwise.  

## Overview

deq is a Haskell implementation of the polynomial-time decision procedure for deterministic register automata reported in the [paper](https://doi.org/10.4230/LIPIcs.MFCS.2018.72) "Polynomial-Time Equivalence Testing for Deterministic Fresh-Register Automata" by Andrzej Murawski, Steven Ramsay and Nikos Tzevelekos.  Permutation group membership is decided using the library [HaskellForMaths](http://hackage.haskell.org/package/HaskellForMaths).

This class of automata read finite words whose letters are pairs `(t,a)` where:
* `t` is drawn from a finite set of tags
* `a` is drawn from an infinite alphabet

The infinite alphabet is deliberately left abstract.  This class of automata are not able to detect any particular letter but merely whether or not two given letters are identical.

## Input Format

The input to the problem is a pair of DRA, specified using two XML files.  Examples can be found in the `examples` subdirectory of the repository.
Each XML file consists of a top-level tag `<dra>` with children `<states>`, `<initial-state>` and `<transitions>`.
Well-formedness can be checked with respect to the DTD `dra.dtd`.

### States

The states of the automaton are specified as a list of `<state>` nodes.  Each node should specify the name of the state and a list of available registers using the children `<id>` and `<available-registers>` respectively.

The available registers specify the registers that are in scope for transitions that start from that state.
They are specified by a list of `<register>` nodes, each containing the number (positive integer) of some register.

```
    <state>
      <id>q3</id>
      <available-registers>
        <register>1</register>
        <register>2</register>
      </available-registers>
    </state>
```

### Initial state

The initial state of the automaton is specified using the tag `<initial-state>` whose contents should be one of the states specified in the sibling node `<states>`.
```
<initial-state>q1</initial-state>
```
It is a requirement that the initial state has _no active registers_.

### Transitions

Transitions move the automaton from state to state and are specified by the tag `<transitions>`.
Each transition is listed as a child of this node using the tag `<transition>`.

There are three kinds of transition, whose applicability is predicated upon the next letter `(t, a)` of the input and the state (or history) of the registers:
* `Stored i` - is applicable if `a` is currently stored in register `i`.
* `LFresh i` - is applicable if `a` is not currently stored in any register.
* `GFresh i` - is applicable if `a` has _never_ been stored in any register.

If a transition on `LFresh i` or `GFresh i` is taken, then `a` is stored in register `i`.

Additionally, every transition must specify a tag `T` and, for a transition to be applicable on input `(t, a)`, it must also be the case that `t = T`.

The XML following XML nodes are used to specify a transition: 
* `<from>` : the starting state
* `<input>` : the tag `T`
* `<op>` : the mode, either `Stored`, `LFresh` or `GFresh`
* `<register>` : the associated register `i`
* `<to>` : the ending state

```
   <transition>
      <from>q1</from>
      <input>push</input>
      <op>LFresh</op>
      <register>1</register>
      <to>q2</to>
    </transition>
```

There are a number of requirements on well-formedness.  
* To ensure that the automaton is _deterministic_ the following must be observed:    
  * For each combination of starting state and tag, there can be at most one transition whose mode is _either_ `LFresh` or `GFresh`.  
  * For each combination of starting state, tag and register, there can be at most one transition with mode `Stored`.  
* To ensure that proper register discipline is followed:
  * On a transition from state `q1` to state `q2` on `Stored i`, the available registers of state `q2` must be contained in the available registers of state `q1`.
  * On a transition from state `q1` to state `q2` on `LFresh i` or `GFresh i`, each available register of state `q2` must either be `i` or an available register of state `q1`.
