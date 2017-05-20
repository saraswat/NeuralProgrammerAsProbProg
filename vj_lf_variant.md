# Semantic forms

Basic idea is to stack with using pure lambda terms as semantic forms, and see if we can make this work. 
(Hopefully this setting is simple enough we don't need to go to glue.)

```
(Lambda terms) M ::= c | x | M@M | x\M
```
 
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

