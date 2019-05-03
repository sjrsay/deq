// cut out from learning.cpp
#include <stdlib.h>
#include <sys/time.h>

#include "../include/loisextra.h"

#include <iostream>
#include <string>
using namespace std;
using namespace lois;

long long getVa() {
  struct timeval tval;
  gettimeofday(&tval, NULL);
  return tval.tv_sec * 1000000 + tval.tv_usec;
  }

long long lasttime;

void showTimeElapsed() {
  long long curtime = getVa();

  printf("%.0f", (curtime-lasttime)/1000.0);

  lasttime = curtime;
  }

#include "../include/lois-weak.h"

typedef elem Symbol;
typedef elem State;

typedef lvector<Symbol> word;


ostream& operator << (ostream& os, const word& w) {
  for(auto s: w) os << s;
  return os;
  }

#include "../include/lois-automaton.h"
typedef automaton<State, Symbol> myautomaton;
typedef transition<State, Symbol> mytransition;

word concat(word x, word y) {
  word res;
  for(Symbol a: x) res.push_back(a);
  for(Symbol b: y) res.push_back(b);
  return res;
  }

word concat(Symbol a, word y) {
  word res;
  res.push_back(a);
  for(Symbol b: y) res.push_back(b);
  return res;
  }

word concat(word x, Symbol b) {
  word res;
  for(Symbol a: x) res.push_back(a);
  res.push_back(b);
  return res;
  }

word concat(word x, word y, word z) {
  word res;
  for(Symbol a: x) res.push_back(a);
  for(Symbol b: y) res.push_back(b);
  for(Symbol c: z) res.push_back(c);
  return res;
  }

word concat(word x, Symbol b, word z) {
  word res;
  for(Symbol a: x) res.push_back(a);
  res.push_back(b);
  for(Symbol c: z) res.push_back(c);
  return res;
  }

lbool operator ^ (lbool x, lbool y) {
  return (x&&!y) || (y&&!x);
  }



template<class T> rbool invector(const T& a, const lvector<T>& v) {
    lbool exists = false;
    for(auto& t: v) exists |= (a == t);
    return exists;
}

template<class T> rbool notinvector(const T& a, const lvector<T>& v) {
    lbool all = true;
    for(auto& t: v) all &= (a != t);
    return all;
}


void printAutomaton(const myautomaton& L) {
  std::cout << "Q = " << L.Q << std::endl;
  std::cout << "I = " << L.I << std::endl;
  std::cout << "F = " << L.F << std::endl;
  std::cout << "δ = " << L.delta << std::endl;
  }

bool eqtest(const myautomaton& L1, const myautomaton& L2) {

  // (q1,q2) \in compare iff, after reading some word, the
  // DFA L is in state q1 and the DFA Learned
  // is in state q2
  lsetof<lpair<State, State>> compare;

  // for each (q1,q2) in compare, witnesses contains ((q1,q2), w),
  // where w is the witness word
  lsetof<lpair<lpair<State,State>, word> > witnesses;

  for(elem e: L1.I) for(elem e2: L2.I) {
    auto initpair = make_lpair(e, e2);
    compare += initpair;
    witnesses += make_lpair(initpair, word());
    }

  for(auto witness: witnesses) {

    elem q1 = (witness.first).first;
    elem q2 = (witness.first).second;

    word w = (witness.second);

    lbool m1 = memberof(q1, L1.F);
    lbool m2 = memberof(q2, L2.F);

    If(m1 ^ m2) {
      return false;
      }

    for(auto a: L1.alph)
      for(mytransition& t1: L1.delta)
        If(t1.src == q1 && t1.symbol == a)
      for(mytransition& t2: L2.delta)
        If(t2.src == q2 && t2.symbol == a) {
          auto p2 = make_lpair(t1.tgt, t2.tgt);
          If(!memberof(p2, compare)) {
            compare += p2;
            witnesses += make_lpair(p2, concat(w, a));
            }
          }
    }

return true;
}

template<class T, class U> elem elpair(T x, U y) { return elof(make_lpair(x,y)); }

void build_RL_S_StackAutomaton(myautomaton& target, lsetof<term>& A, elem& etrash, elem& epush, elem& epop, word w, int more) {

    auto ew = elof(w);
    target.Q += elof(w);
    target.F += elof(w);

    if(more) {
        for(term a: A) {

            Ife (notinvector(elof(a),w)) {
                word w2 = concat(elof(a),w);
                auto ew2 = elof(w2);

                build_RL_S_StackAutomaton(target, A, etrash, epush, epop, w2, more-1);


                target.delta += mytransition(ew, elof(make_lpair(epush, a)), ew2);
                target.delta += mytransition(ew2, elof(make_lpair(epop, a)), ew);

                for(term b: A) If(a != b)
                    target.delta += mytransition(elof(w2), (elpair(epop, b)), etrash);
            }
            else target.delta += mytransition(ew, elof(make_lpair(epush, a)), etrash);
        }
    }
    else for(term a: A) target.delta += mytransition(elof(w), (elpair(epush, a)), etrash);
}





void build_LR_M_StackAutomaton(myautomaton& target, lsetof<term>& A, elem& etrash, elem& epush, elem& epop, word w, int more) {

  auto ew = elof(w);
  target.Q += elof(w);
  target.F += elof(w);

    if(more) {
        for(term a: A) {

            {
            word w2 = concat(w, elof(a));
            auto ew2 = elof(w2);

            build_LR_M_StackAutomaton(target, A, etrash, epush, epop, w2, more-1);

            target.delta += mytransition(ew, elof(make_lpair(epush, a)), ew2);
            target.delta += mytransition(ew2, elof(make_lpair(epop, a)), ew);

            for(term b: A) If(a != b)
                target.delta += mytransition(elof(w2), (elpair(epop, b)), etrash);
            }
        }
    }
    else for(term a: A) target.delta += mytransition(elof(w), (elpair(epush, a)), etrash);
}


void build_LR_S_StackAutomaton(myautomaton& target, lsetof<term>& A, elem& etrash, elem& epush, elem& epop, word w, int more) {

    auto ew = elof(w);
    target.Q += elof(w);
    target.F += elof(w);

    if(more) {
        for(term a: A) {

            Ife (notinvector(elof(a),w)) {
                word w2 = concat(w, elof(a));
                auto ew2 = elof(w2);

                build_LR_S_StackAutomaton(target, A, etrash, epush, epop, w2, more-1);


                target.delta += mytransition(ew, elof(make_lpair(epush, a)), ew2);
                target.delta += mytransition(ew2, elof(make_lpair(epop, a)), ew);

                for(term b: A) If(a != b)
                    target.delta += mytransition(elof(w2), (elpair(epop, b)), etrash);
            }
            else target.delta += mytransition(ew, elof(make_lpair(epush, a)), etrash);
        }
    }
    else for(term a: A) target.delta += mytransition(elof(w), (elpair(epush, a)), etrash);
}


void build_RL_M_StackAutomaton(myautomaton& target, lsetof<term>& A, elem& etrash, elem& epush, elem& epop, word w, int more) {

    auto ew = elof(w);
    target.Q += elof(w);
    target.F += elof(w);

    if(more) {
        for(term a: A) {
            {
            word w2 = concat(elof(a),w);
            auto ew2 = elof(w2);

            build_RL_M_StackAutomaton(target, A, etrash, epush, epop, w2, more-1);

            target.delta += mytransition(ew, elof(make_lpair(epush, a)), ew2);
            target.delta += mytransition(ew2, elof(make_lpair(epop, a)), ew);

            for(term b: A) If(a != b)
                target.delta += mytransition(elof(w2), (elpair(epop, b)), etrash);
            }
        }
    }
    else for(term a: A) target.delta += mytransition(elof(w), (elpair(epush, a)), etrash);
}



void buildloopLR(myautomaton& target, lsetof<term>& A, elem& etrash, elem& epush, elem& epop, int i) {

    word w0  =  word();
    auto ew0 = elof(w0);
    target.Q += elof(w0);
    target.F += elof(w0);

    switch(i) {
case 1:
#include "st1"
break;
case 2:
#include "st2"
break;
case 3:
#include "st3"
break;
case 4:
#include "st4"
break;
case 5:
#include "st5"
break;
case 6:
#include "st6"
break;
case 7:
#include "st7"
break;
case 8:
#include "st8"
break;
case 9:
#include "st9"
break;
case 10:
#include "st10"
break;
case 11:
#include "st11"
break;
case 12:
#include "st12"
break;
case 13:
#include "st13"
break;
case 14:
#include "st14"
break;
}
}

void buildloopRL(myautomaton& target, lsetof<term>& A, elem& etrash, elem& epush, elem& epop, int i) {

    word w0  =  word();
    auto ew0 = elof(w0);
    target.Q += elof(w0);
    target.F += elof(w0);

    switch(i) {
case 1:
#include "ts1"
break;
case 2:
#include "ts2"
break;
case 3:
#include "ts3"
break;
case 4:
#include "ts4"
break;
case 5:
#include "ts5"
break;
case 6:
#include "ts6"
break;
case 7:
#include "ts7"
break;
case 8:
#include "ts8"
break;
case 9:
#include "ts9"
break;
case 10:
#include "ts10"
break;
case 11:
#include "ts11"
break;
case 12:
#include "ts12"
break;
case 13:
#include "ts13"
break;
case 14:
#include "ts14"
break;

    }
}


template<class T> lset be_lset(const lsetof<T>& s) {
  lset val;
  for(auto el: s) val += elof(el);
  return val;
  }

int main() {
  initLois();

  Domain dA("Atoms");
  auto A = dA.getSet();

  lset sigma;
  for(auto a: A) sigma += elof(a);


    int i = 1;
    
    while (i < 11) {
        myautomaton target0;
            elem etrash = elof(0);
            elem epush = elof(1);
            elem epop = elof(2);
            target0.alph = be_lset(newSet(epush, epop) * A);
            target0.Q += etrash;
            target0.I += elof(word());
            for(auto a: A) target0.delta += mytransition(elof(word()), elpair(epop, elof(a)), etrash);

            buildloopLR(target0, A, etrash, epush, epop, i);
        
        int j = 1;
        while (j < 11) {
            myautomaton target1;
                elem etrash = elof(0);
                elem epush = elof(1);
                elem epop = elof(2);
                target1.alph = be_lset(newSet(epush, epop) * A);
                target1.Q += etrash;
                target1.I += elof(word());
                for(auto a: A) target1.delta += mytransition(elof(word()), elpair(epop, elof(a)), etrash);

                buildloopRL(target1, A, etrash, epush, epop, j);

            std::cout << i << ", " << j << ", ";
            lasttime = getVa();
            bool b = eqtest(target0,target1);
            std::cout << (b ? "YES, " : "NO, ");
            showTimeElapsed();
            std::cout << std::endl;
            j++;
        }
        i++;
  }

  return 0;
  }
