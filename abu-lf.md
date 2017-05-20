# Description of the grammar formalism

(From Abu, with my inline comments)

The logical formalism was just Datalog, the same used in Geoquery and Jobs (although it's interconvertible between lambda calculus in my code, I had implemented the transformation functions with Datalog when I first started, so just continued using it). So the abstract syntax is something like:

```
E ::= \+E | (E,E,...,E) | PRED(V) | PRED(V,V) | PRED(V,V,V) | FUNC(x,E) | FUNC(x,y,E) | FUNC(x,E,y)
FUNC ::= answer, count, sum, highest, lowest, longest, shortest, largest, smallest, most, fewest
V ::= x, CONSTANT, INTEGER, INTERNAL_PRED(CONSTANT)
```
`\+` is logical negation, `x` and `y` are variables, `PRED` is a predicate, `INTERNAL_PRED` only showed up in Geoquery for constructs like `stateid(texas)`.


So the file has two kinds of statements: _nonterminal declarations_, and _production rules_.

## Nonterminal declarations
The line:
```
SQ nonterminal {has_function_answer, first_predicate, second_predicate, third_predicate} {10000000.0, 10.0, 10.0, 100.0, 100.0} {1.0, 1.0, 0.1, 1.0, 1.0} {} 10 1.0 {}
```
is a nonterminal declaration. The first list is the set of semantic features that are used to probabilistically select 
the production rule to expand `SQ`. If we're using categorical distributions in the first run, we don't need these.

The next two lists of numbers are hyperparameters for the HDP: We place a gamma prior on the concentration parameter
\alpha for each row of the HDP. By construction of the HDP hierarchy, `num(rows) = num(semantic features) + 1`. 
The first list is the set of shape parameters in the gamma prior, and the second list is the set of rate parameters.

The list after that (empty in this example) is the set of _excluded tokens_, which are only used by preterminals. 
The `V` preterminal is a good example, where `than`, `that`, ... are listed, and so the rules `V -> "than"`, 
`V -> "that"`, ... have zero prior probability.

Following is a positive integer and a parameter in [0,1]. These are also only used by preterminals. I allow 
preterminals to contain rules with multiple tokens, such as `V -> "run through"`, `V -> "look up"`, `N -> "New York"`, 
etc. To do so, I use the following prior on the terminal production rules: Sample the first token uniformly at 
random from the set of all tokens for that preterminal. Next, with probability _p_, we stop generating this rule. 
Otherwise, we sample the next token uniformly at random, and repeat until we stop. The first integer specifies 
how many tokens there are for that nonterminal, and the second real-valued parameter is _p_ the stop probability. 
So for example, the `V` preterminal has `10000` tokens and stop probability `0.95`.

We can ignore the last list since I barely use it. I don't think it's even necessary.

## Productions

So deleting elements of the parent semantic form effectively captures the principle of compositionality 
and helps generalization. We eventually want the logical form at the leaves (`N`, `V`, `ADJ`, etc) 
to be much simpler, such as individual predicate instances, rather than the full logical form.

I define the semantic transformations in my implementation of the logical formalism. Since we're using a different logical formalism, we will need to change these functions. The code is a bit dense, so I'll define them here:
  1. `delete_answer` - for a logical form `x\f(x)`, return `f(x)`. If there is no leading lambda term, then fail.
      _vj -- I believe this means: `F` in the head, `F@X` in the body, for a new variable `X`._
  2. `delete_count` - for a logical form `count(x,f(x),y)`, return `f(x)`. If there is no `count` function, then fail. 
  Semantically, `count(x,f(x),y)` is true if `y = #{x:f(x)}` but the grammar doesn't need to know this.
      _vj -- I believe this means: `count(X,B,Y)` in the head, `B` in the body._
  3. `delete_not` - for a logical form `not f(x)`, return `f(x)`. If there is no negation, then fail.
      _vj -- I believe this means: `not F` in the head, `F` in the body._
  4. `delete_function` - for a logical form `G(x,f(x),...)` with any function `G` (i.e. `max`, `min`, `count`, `not`, etc), 
  return `f(x)`. If there is no function, then fail.
        vj -- I believe this means: `G(F,...)` in the head, `F@X` in the body.
  5. `delete_head` - since this grammar was built for a logical formalism with variables, we need to model how 
  variables are passed to the child logical forms (so that it's not ambiguous how to reverse the process during parsing). 
  So every logical form has a unique "head" variable (or no variables). So suppose we have the logical form 
  `(f(x,y,z) and g(y,z))`, and the head is `x`. If we delete the left predicate, the remaining logical form will have a 
  new head y. During parsing, the parser will have two logical forms `f(x,y,z)`, `g(x,y)` and there are a combinatorial 
  number of ways to relabel the variables when combining the two. This is where `delete_head` and `disjoint` transformations 
  come in.
    1. If the transformation is `select_left` and `delete_left_head`, the combination will be `(f(x,y,z) and g(y,z))`.
    2. If the transformation was instead `select_left` `delete_left`, the combination would be `(f(x,y,z) and g(x,y))`.
    3. If the transformation was `select_left_disjoint delete_left_disjoint`, the combination would be `(f(x,y,z) and g(x,w))`.
    4. If the transformation was `select_left_disjoint delete_left_head_disjoint`, the result would be `(f(x,y,z) and g(y,w))`.
  6. `select_function` - for a logical form `G(x,f(x),...)` with any function `G`, return `G`. If there is no function, fail.
  7. `select_left` - for a logical form `A and B and ...`, return `A`.

Some of these are also labeled `keep_function` which takes input `G(x,A and B and ...)` and returns `G(x,A)` for any 
function `G`.

We compose a bunch of these in a lot of rules. For example, `select_left3_delete_answer_disjoint` first does 
`delete_answer` and then does `select_left3_disjoint`.

That should cover most of them. Let me know if you have any questions.


Aside from 3 or so examples in Jobs, there was no disjunction. The transformation functions for coordination (the ones with CNJ) operate on the conjunction forms (E,E,...,E) which is why I left out the operator since it's implicit.

```
DP -> DP:select_left2_disjoint CNJ:null DP:delete_left2_disjoint
```
`select_left2` is the same as `select_left` except it selects the two left-most terms instead of one. The "disjoint" means 
that the variables in each child don't overlap, except for the head variable. So this operation would applicable to a 
logical form like: `f1(A,B),f2(B,C),f3(A,D,E),f4(A,D,F)` where the head is `A`. `select_left_disjoint` and 
`delete_left_disjoint` would not work since `B` would exist in both logical forms. Note that if `delete_head` 
does not appear in the transformation, then it expects the head variable to appear in the child logical form.

```
DP -> DP:select_left_delete_head_disjoint CNJ:null DP:delete_left_disjoint
```
This is the same as `select_left_disjoint` and `delete_left_disjoint` except the head does not appear in the left child's 
logical form. `delete_head` expects that the child logical form does not have the head variable. So this transformation 
would be applicable to: `f1(B,C),f2(C,D),f3(A,D,E),f4(A,D,F)`. The left child would be `f1(A,B)` and the right child would 
be `f2(C,D),f3(A,D,E),f4(A,D,F)`.
