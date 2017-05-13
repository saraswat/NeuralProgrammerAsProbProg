# NeuralProgrammerAsProbProg
Goal is to work with the examples in [Arvind Neelakantan's work](https://arxiv.org/abs/1611.08945) and understand them as examples we could represent in Probabilistic CC (and implement via translation to [PRISM](http://rjida.meijo-u.ac.jp/sato-www/prism/)). And if we cannot, why not -- this should help us understand the practical limitations of Probabilistic CC vs neural nets. 

For now, we will approximate PCC with definite clauses that have a fixed (left to right) order of evaluation, and ensure that our programs are such that atoms `cond` used in a sample operator `X | cond ~ PD` are ground when executed.

The overall problem to be solved: Design a system that can take as input
  1. an utterance
  2. a table
and compute an answer to the question in the utterance, using only the information in the table. The number of columns of the table and their header and row information can vary from question to question. Cell entries may have numbers or multiple words. The training set available is a corpus `(x_i, t_i, a_i)_i` where `x_i` is the utterance, `t_i` a table and `a_i` is the answer. The program is latent.

The basic approach is to augment a probabilistic CCP semantic parser with an evaluator of the logical form.
```prolog
result(Query, Table, Ans):- parse(Query, Form), eval(Form, Table, Ans).
```
`parse/2` is intended to be a "standard" semantic parser that converts the input query into a logical form, e.g. using Abu's technique (stochastic definite clause grammar, with learning of terminal productions, and with Hierarchical Dirichlet Process priors). The evaluator, `eval(Form, Table, Ans)` treats `Form` as a program, evaluated against `Table` to produce `Ans`. The program is deterministic for the most part, but some complications noted below may be handled by letting the evaluator be probabilistic, and learning the probability distribution from data. 

Note that `Form` does not occur in the head of the clause -- it is "latent". Training is performed on a bunch of ground ``result/3`` triples. At test time, `Query` and `Table` are instantiated, and `Ans` is unknown and computed by the program. We will use [Cussen's Failure Adjusted Maximisation (FAM) algorithm](http://link.springer.com/article/10.1023/A:1010924021315) (based on EM) for training. We will use the Viterbi algorithm in PRISM to compute the most probable solution. Both these techniques are implemented in [PRISM](http://rjida.meijo-u.ac.jp/sato-www/prism/).

_Q for Abu: Is your implemented system using techniques similar to PRISM's Viterbi training (see [5]) + generalized inside-out algorithm [see [2]), or are there different ideas?_

Note that to help the training process it may make sense to augment the training set with some `parse/2` pairs, and some `eval/3` triples. It will be interesting to determine how overall performance improves with these augmentations. 

The key to this approach is the design of the logical form. One idea is to work with Arvind's formalization of the programming model. This will give us some experience on the basis of which we can figure out if another programming model may make more sense. 

An advantage of this approach is a clean separation between parsing (understanding the nuances of the natural language utterance and generating the logical form) and evaluation (using information in the logical form to obtain an answer, given the table). The critical learning problem is going to be whether the information gleaned from the input can provide strong guidance on the probabilities. This is the central problem probabilistic parsers solve -- the key question here is whether the search implicit in the determination of `Form` for a given `(Query, Table, Ans)` triple is tractable. Perhaps it will be important to go to a true CCP context, with parallel execution of `parse(Query, Form)` and `eval(Form, Table, Ans)` agents, so that partial `Form`s that could not possibly lead to the right `Ans` fail early. 

_Q: What is the analog of [Arvind's](https://arxiv.org/abs/1611.08945) "soft selection", during training for PCC?_

##Complications

Note the answer may be a number that occurs directly in the table. We will represent this by permitting the evaluator to be non-deterministic -- choose either the number in the table, or choose the computed answer -- and let the choice be conditioned by the logical form, and then letting training determine the probability distribution.

## Decisions to be made.
In this approach, `parse/2` is a probabilistic CC parser, hence `Form` is a symbolic expression and training is performed by variants of EM. 

A completely different approach is to develop a differentiable architecture, as in [1], and train end to end using SGD. Again, this approach may be worth pursuing in the context of Jianpeng Cheng's system [6] -- replacing the "Question RNN" in [1] with JP's semantic parser. 

Yet another alternative is to use [4] as a semantic parser. But here the gap between the form output by the semantic parser and the form needed for execution will be significant, and will need to be bridged by a learner. 


# References
1. Arvind Neelakantan, Quoc V. Le, Martin Abadi, Andrew McCallum, Dario Amode. [Learning a Natural Language Interface with Neural Programmer.](https://arxiv.org/abs/1606.04474) ICLR 2017.
2. Taisuke Sato. [PRISM Manual.](rjida.meijo-u.ac.jp/prism/download/prism21.pdf)
3. James Cussens. [Parameter Estimation in Stochastic Logic Programs.](http://link.springer.com/article/10.1023/A:1010924021315) _Machine Learning_. September 2001, Volume 44, Issue 3, pp 245–271
4. Siva Reddy, Oscar Täckström, Slav Petrov, Mark Steedman, Mirella Lapata. [Universal Semantic Parsing.](https://arxiv.org/abs/1702.03196)
5. Teisuke Sato and Keiichi Kubota. [Viterbi training in PRISM](https://www.semanticscholar.org/paper/Viterbi-training-in-PRISM-Sato-Kubota/92756666eff7dbac73ceb4b8b398e4ae61f33d7f). TPLP, 2015, pp 147--168.
6. Jianpeng Cheng, Siva Reddy, Vijay Saraswat and Mirella Lapata. Learning Structured Natural Language Representations for Semantic Parsing. ACL 2017
