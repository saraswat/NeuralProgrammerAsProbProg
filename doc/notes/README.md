# logic-nn
## Doing logic with neural nets.

We want to rebuild logic on a machine learning foundation.

Consider the [inductive logic programming](https://en.wikipedia.org/wiki/Inductive_logic_programming) situation. We are given some logical vocabulary `L`, a background theory `S` in `L`, and a collection of "observations", `O`, that have somehow been made in the real world. It is expected that the observations are noisy. For now, let us say the observations are simply sets of positive and negative literals `(O+,O-)` for a chosen predicate `p/k`. 

`S` is such that it alone cannot "explain" the given `O+` and `O-`. We desire to learn a theory `T` s.t. `S` and `T` are jointly consistent, can entail `O+`, and is not inconsistent with `O-`. 

The standard development of ILP goes through certain symbolic and search techniques where different (definite clause) theories `T` are constructed and tried. 

We want to try something different, using techniques such as stochastic gradient descent that have proved very powerful for neural networks. 

SGD: The basic idea is that you define a function `f(theta)`, parametrized by `theta`, and a loss function `L`. The setting is supervised, i.e. you are given a possibly large set of obsverations `O` of the form `(x,y)`, where each of the `x` and `y` can be tuples of a given fixed size. You are looking for a setting of the parameters `theta` which minimizes the loss function. Gradient descent proceeds by assuming a current value `u` of the parameters `theta`, using it to compute the loss `L(y, f(u)(x))` and then propagate the loss back through the function `f`, using the partial derivatives `do f/do j` for each parameter `j` in `theta`, to update the parameters. Repeat across all observations. For various choices of the function `f`, e.g. feed-forward neural nets, recurrent neural nets this technique has proved remarkably robust in practice, across a wide variety of data-sets, e.g. in vision, and in speech, and in some natural language tasks such as machine translation, text generation etc. 

So we are looking for some kind of continuous embedding of logical formulas: (For now consider the underlying logic is untyped, in practice we will want to use types)
1. Some space `B` for the interpretation of Boolean formulas, with interpretations for the logical connectives (conjunction, disjunction, negation, implication -- we should experiment with both intuitionistic and classical interpretations).
2. An embedding `I(c)` of individual constant symbols `c` into some space `U` of interpretations. (Similarly for function symbols.)
3. An embedding of the all and some quantifiers, `all:(U->B)->B`, `some:(U->B)->B`
3. An embedding `I(p/k)` of individual predicate symbols `p/k` into some space `P{k}` that is roughly `(U^k ->B)`
4. An embedding of predication: for every element `p` of `P{k}`, an application function `A(p)` that takes a tuple `u` in `U^k` to a value in `B`

The parameters (which we wish to learn) are the values `I(c)` for some of the constant and function symbols, and `I(p/k)` for some of the predicate symbols. The loss function can be taken to be a "margin loss" -- measure the difference between a positive and a negative tuple for the given predicate `p/k`, and we seek to maximize it. (Other loss functions are possible.)



The work on LSTMs for textual entailment establishes that they can be used for single step entailment.
Can they be used for multi-step entailments, i.e. to implement reasoning, in a first order setting.
Can we build theories into NN learners?

## References
1. [Bishan Yang's papers:](http://arxiv.org/abs/1412.6575), [more](http://arxiv.org/abs/1412.6575)
2. [Tim's injecting paper:](http://rockt.github.io/pdf/rocktaschel2015injecting.pdf)

(See reference lists of these papers for other papers.)
