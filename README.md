#  Neural Programmer as Probabilistic Programming, and other approaches
Goal is to work with the examples in [Arvind Neelakantan's work](https://arxiv.org/abs/1611.08945) and understand how to use other semantic parsing techniques to improve performance. 

One main line of attack is to use [Probabilistic CC](https://github.com/saraswat/pcc) (and implement via translation to [PRISM](http://rjida.meijo-u.ac.jp/sato-www/prism/)).  For now, we will approximate PCC with definite clauses that have a fixed (left to right) order of evaluation, and ensure that our programs are such that atoms `cond` used in a sample operator `X | cond ~ PD` are ground when executed.

The overall problem to be solved: Design a system that can take as input an utterance and a table
and computes an answer to the question in the utterance, using only the information in the table. The number of columns of the table and their header and row information can vary from question to question. Entries in the table (cells) may have numbers or multiple words. The training set available is a corpus `(x_i, t_i, a_i)_i` where `x_i` is the utterance, `t_i` a table and `a_i` is the answer. The program is latent. The corpus is described in [7]. Note that with 37% results, there is considerable room for improvement!

The basic approach is to augment a probabilistic CCP semantic parser `parse/2` with an evaluator of the logical form.
```prolog
result(Query, Table, Ans):- parse(Query, Form), eval(Form, Table, Ans).
```
`parse/2` is intended to be a "standard" semantic parser that converts the input query into a logical form, e.g. using Abu's technique (stochastic definite clause grammar, with learning of terminal productions, and with Hierarchical Dirichlet Process priors). The evaluator, `eval(Form, Table, Ans)` treats `Form` as a program, evaluated against `Table` to produce `Ans`. The program is deterministic for the most part, but some complications noted below may be handled by letting the evaluator be probabilistic, and learning the probability distribution from data. 

Note that `Form` does not occur in the head of the clause -- it is "latent". Training is performed on a bunch of ground ``result/3`` triples. At test time, `Query` and `Table` are instantiated, and `Ans` is unknown and computed by the program. We will use [Cussen's Failure Adjusted Maximisation (FAM) algorithm](http://link.springer.com/article/10.1023/A:1010924021315) (based on EM) for training. We will use the Viterbi algorithm in PRISM to compute the most probable solution. Both these techniques are implemented in [PRISM](http://rjida.meijo-u.ac.jp/sato-www/prism/).

_Q for Abu: Is your implemented system using techniques similar to PRISM's Viterbi training (see [5]) + generalized inside-out algorithm [see [2]), or are there different ideas? [5] contains a discussion of statistical parsing in this context. Note that PRISM implements a number of other inferencing techniques, including Variational Bayes, that may be of interest here._

The key to this approach is the design of the logical form. The language of logical forms should be _expressive_ -- rich enough to express all functions from columns to values that can be specified by users in an utterance. It should attempt to be _orthogonal_ in that given a set of column names, and a function from tables with those column names to values, the number of programs in the language that express that function is very small, preferrably one. 

A design in the variable-free "FunQL" style is given below. For this language, `eval(Form, Table, ?)` is determinate -- given a `Form` and a `Table`, the query will produce at most one answer determinately. 

The key to this problem is developing a trainable semantic parser for the given utterance and logical language. Any parser can be used to solve the problem provided that it can generate a small ordered set of candidate parses that contains the "correct" parse, and improve its performance with feedback (generating fewer parses, including the correct parse). Since the input language is conversational (rather than formal), the parser has to exhibit some flexibility in word order. It will also need to be exhibit some genericity with respect to its lexicon because column names will be known only at runtime (may never have been seen during training). 

Parsers such as Li Dong's sequence to sequence parsers are not directly applicable since they need the semantic form for training, and cannot generate a set of candidate logic forms. _But perhaps they can be modified?_

Probabilistic semantic parsers that permit productions to be generated on the fly during training, and that can adjust probabilities during training should be good candidates. 

Note that to help the training process it may make sense to augment the training set with some `parse/2` pairs and some `eval/3` triples (if `eval/3` is probabilistic). It will be interesting to determine how overall performance improves with these augmentations. 

An advantage of this approach is a clean separation between parsing (understanding the nuances of the natural language utterance and generating the logical form) and evaluation (using information in the logical form to obtain an answer, given the table). Note that it may be interesting to run `parse/2` and `eval/3` in parallel, so that feedback from `eval/3` can be used to reject a partial parse (fail early). 


_Q: What is the analog of [Arvind's](https://arxiv.org/abs/1611.08945) "soft selection", during training for PCC?_

## Complications

Note the answer may be a number that occurs directly in the table. We will represent this by permitting the evaluator to be non-deterministic -- choose either the number in the table, or choose the computed answer -- and let the choice be conditioned by the logical form, and then letting training determine the probability distribution.

## Other Approaches
In this approach, `parse/2` is a probabilistic CC parser, hence `Form` is a symbolic expression and training is performed by variants of EM. 

A completely different approach is to develop a differentiable architecture, as in [1], and train end to end using SGD. A key question here is the representation of the utterance. In [1] this is done with a "Question RNN" whose weights are updated during training, presumably leading to an application specific abstraction of the utterance being learnt. Jianpeng has developed an end-to-end differentiable system [6] (based on RNN grammars) which produces a semantic parse in two steps, first generating an "ungrounded" semantic representation, and second learning the grounded lexicon (mapping from natural language predicates to pre-fixed vocabulary of grounded predicates). It may be worth considering replacing the Question RNN in the architecture of [1] with the RNN grammar based component of [6], modifying the rest of the system to accept the question representation as a symbolic expression, but continuing to train end-to-end with SGD. The key conceptual difference from [1] is that the problem is decomposed into translating the utterance into a (symbolic) semantic form and then evaluating that form against the table. The symbolic representation should be much easier to understand -- useful for debugging and explanation. 

Yet another alternative is to use [4] as a semantic parser. But here the gap between the form output by the semantic parser and the form needed for execution will be significant, and will need to be bridged by a separate learner, akin to phase II of [6]. 

This is also a good (but advanced example) for the "Differentiable Logic" project. 

# Initial cut at logical form
The language of logical forms is the set of first order expressions obtained by using the operations given below. This is not dissimilar to the lambda-DCS language presented in [7].

## Type system
Given a table with many rows, and columns with colnames. Each row has an index. 
  1. `Value`  -- an integer, boolean, date, ...
  2. `Values` -- sets of integers, booleans, dates
  3. `Rows`   -- subsequence of rows from given table
  4. `ColName` -- names of columns in the given table

No operations are available on the type `ColName`. Therefore the only terms of this type are constants.
## Operations

### Comparison-based selection
```
op: Colname -> Value -> Rows -> Rows
ge(colname, val, r): subsequence of rows in r whose colname cells have value >= val
gt(colname, val, r)
le(colname, val, r)
lt(colname, val, r)
eq(colname, val, r)
```
Return the subsequence of rows from `r` in which the value of the cell at `colname` has the given relationship to `val`. `colname`, `val` and `r` are evaluated in turn.  `val` must evaluate to a singleton value; for this operations such as `first` or `last` may be used, perhaps together with `proj`. (See examples in the table below.)

`eq(colname, val, r)` is written as `colname(cellvalue, rows)`. 

### Superlatives

```
max, min: ColName -> Rows -> Rows
max(colname, rows)	      subsequence of rows with the highest cell values for colname
min(colname, rows)	      subsequence of rows with the lowest cell values for colname
```

### Navigation

```
prev, next: Rows -> Rows
first,last: Rows -> Row
```

`prev(r)` (`next(r)`) returns the subsequence of rows obtained by taking the preceding (succeeding) row in the original table (if it exists) for each row in `r`. `first(r)` (`last(r)`) returns the first (last) row in `r`. 

### Projection
```
proj: ColName -> Rows -> Values
```

`proj(colname, r)` is written `colname(r)`. Returns the sequence of _values_ obtained by selecting the value of `colname` for each row in `r`.

### Numeric operations
``` 
+, -, *, /: Values -> Values -> Values
+, -, *, /: Value -> Value -> Value
```

When applied to `Values`, the operators require that the two arguments have the same number of elements, the operator is applied to corresponding elements.

### Set operations
```
all: Rows
either: Rows -> Rows -> Rows
```
`all` is the set of all rows. 

`either(rs, qs)` contains the rows of `rs` and `qs` in the sequence in which they occur in the original table. (Example: `either(country(china, all), country(france, all))` is the collection of all rows in the table whose `country` column contains `china` or `france`.)
Note that `both(rs, qs)` should not be needed. (It contains rows that are in both `rs` and `qs`. Example: `both(country(china, all), city(beijing, all))` is the collection of all rows in the table whose `country` column contains `china` and `city` column contains `beijing`. It can also be expressed as `country(china, city(beijing, all))`.)

### Miscellaneous
```
card: Rows -> Value card(r) is the number of rows in r.
```

# Examples
Consider [a table](https://en.wikipedia.org/wiki/List_of_Olympic_Games_host_cities) given by:

|Year	|City |	Country	| Nations |
| --- | --- | --- | --- |
| 1896 | athens | greece | 14 |
| 1900	| paris | france | 24 |
| 1904  | 'st louis' | usa | 12 |
 | ... | ... | ... | ... |
| 2004 | athens | greece | 201 |
| 2008 | beijing | china | 204 |
| 2012 | london | uk | 204 |



Here are some example questions and their translations.

| Utterance | Form |
| --------- | ---- |
| _Events in Athens_ | `city(athens, all)`|
| _Events in Athens or Beijing_ | `either(city(athens, all), city(beijing, all))`|
| _Events in Athens before 1990_ | `lt(year, 1990, city(athens, all))`|
| _How many events were in Athens, Greece?_   | `card(city(athens, all))` |
| _Events in the same country as Athens_ | `country(country(first(city(athens, all))), all)` | 
| _Greece held its last Summer Olympics in which year?_   | `year(max(year, country(greece, all)))` |
| _In which city's the first time with at least 20 nations?_ | `city(min(year, atleast(nations, 20, all)))` |
|  _Which years have the most participating countries?_ | `years(max(nations, all))` |
|  _How many more participants were there in 1990 than in the first year?_  | `nations(year(1990, all)) - nations(min(year, all))` |

  

# References
1. Arvind Neelakantan, Quoc V. Le, Martin Abadi, Andrew McCallum, Dario Amode. [Learning a Natural Language Interface with Neural Programmer.](https://arxiv.org/abs/1606.04474) ICLR 2017.
2. Taisuke Sato. [PRISM Manual.](rjida.meijo-u.ac.jp/prism/download/prism21.pdf)
3. James Cussens. [Parameter Estimation in Stochastic Logic Programs.](http://link.springer.com/article/10.1023/A:1010924021315) _Machine Learning_. September 2001, Volume 44, Issue 3, pp 245–271
4. Siva Reddy, Oscar Täckström, Slav Petrov, Mark Steedman, Mirella Lapata. [Universal Semantic Parsing.](https://arxiv.org/abs/1702.03196)
5. Teisuke Sato and Keiichi Kubota. [Viterbi training in PRISM](https://www.semanticscholar.org/paper/Viterbi-training-in-PRISM-Sato-Kubota/92756666eff7dbac73ceb4b8b398e4ae61f33d7f). TPLP, 2015, pp 147--168.
6. Jianpeng Cheng, Siva Reddy, Vijay Saraswat and Mirella Lapata. Learning Structured Natural Language Representations for Semantic Parsing. ACL 2017
7. Panupong Pasupat and Percy Liang. [Compositional Semantic Parsing on Semi-Structured Tables.](https://cs.stanford.edu/~pliang/papers/compositional-acl2015.pdf) ACL 2015.
8. Vijay Saraswat. [Probabilistic CCP (logic programming subset)](https://github.com/saraswat/pcc). In progress.
