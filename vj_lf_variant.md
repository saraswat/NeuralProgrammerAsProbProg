# Semantic forms

Basic idea is to stack with using pure lambda terms as semantic forms, and see if we can make this work. 
(Hopefully this setting is simple enough we don't need to go to glue.)

```
(Lambda terms) M ::= c | x | M(M) | M(M,M) | M(M,M,M) | x\M
```
(This differs from the usual presentation in that we explicit draw out two argument and three argument applications.
Reduction rules are appropriately modified:

```
x\M (N)         --> M[N/x]
x\y\M (N,P)     --> M[N/x,P/y]
x\y\z\M (N,P,Q) --> M[N/x,P/y,Q/z]
```
)

Productions will be declared in the form:
``
NT: M1 --> RHS
``

`RHS` is a sequence of terminals and non-terminals, each optionally equipped with a logical form (i.e., suffixed with `:M`). 
Logical forms may have free  variables -- and these may be higher-order. Every higher-order variable that occurs in an LF 
in the body of the production must occur in the head. 

Rules can be read in two ways -- (a) top-down: Given an LF for the head, LF's are (determinately) constructed by the rule for 
each constitutent, satisfying the given declaration for the constitutent, (b) bottom-up: Given LFs for the constituents, the LF
for the head is (determinately) constructed by the rule, satisfying the given declaration. 

For (a) _higher-order unification_ needs to be used. In general HOU is undecidable. However, we can restrict our attention to 
_linear  patterns_ (see Dale Miller et al's work); these can be implemented with variants of first-order unification.

(This we eschew the prescriptive design of Abu's LF in favior of the more traditional declarative design, as in LFG / CCG etc.)

## Grammar

```
DP nonterminal {has_function_not, first_predicate, second_predicate} {1000.0, 0.1, 10000.0, 1.0} {1.0, 1.0, 1.0, 1.0} {} 10 1.0 {}

DP:X -> ALL OF DP:X     #DP -> ALL:null OF:null DP:identity
DP:X -> DEFINITE NP:X   #DP -> DEFINITE:null NP:identity
DP:X -> A NP:X          #DP -> A:null NP:identity
DP:X -> AT_LEAST A NP:X #DP -> AT_LEAST:null A:null NP:identity
DP:X -> ANY NP:X        #DP -> ANY:null NP:identity
DP:X -> SOME NP:X       # DP -> SOME:null NP:identity
DP:not@X -> NO NP:X     #DP -> NO:null NP:delete_not
DP:X -> NP:X            #DP -> NP:identity

DP: x\K(L@x, R@x) -> DP:L CNJ:K DP:R #DP:select_left_disjoint CNJ:null DP:delete_left_disjoint

#DP -> DP:select_left2_disjoint CNJ:null DP:delete_left2_disjoint
DP -> DP:select_left2_disjoint CNJ:null DP:delete_left2_disjoint

#DP -> DP:select_left_delete_head_disjoint CNJ:null DP:delete_left_disjoint
DP -> DP:select_left2_disjoint CNJ:null DP:delete_left2_disjoint
```
