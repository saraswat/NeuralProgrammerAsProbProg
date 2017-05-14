/*
  ColName is of the form cols(C1, ..., CJ). 
  Rows is of the form rows(row(V11,..., V1J), ..., row(VI1,...VIJ)).

  The subsequence of rows is represented as an increasing sequence of indices in 1..J,
  indices([i1, ..., ik]). Hence, they qualify as sets, and operations such as intersection
  can be used with them. (There is no particular reason to keep them sorted, seems this
  will be of use later.)
  
*/

:- use_module(library(lists)).

%% Type checking formulas
% formula(F, T) holds if F is a legal formula, given table T
formula(Form, Table):- formula_values(Form, Table).
formula(Form,_Table):- formula_value(Form).
formula(Form, Table):- formula_rows(Form, Table).
formula(Form, Table):- formula_row(Form, Table).

formula_rows(all, _Table).
formula_rows(either(R,S), Table)       :- formula_rows(R, Table),       formula_rows(S, Table).
formula_rows(both(R, S), Table)        :- formula_rows(R, Table),       formula_rows(S, Table).
formula_rows(ge(Col, _Val, Rows), Table):- formula_colName(Col, Table), formula_rows(Rows, Table).
formula_rows(gt(Col, _Val, Rows), Table):- formula_colName(Col, Table), formula_rows(Rows, Table).
formula_rows(le(Col, _Val, Rows), Table):- formula_colName(Col, Table), formula_rows(Rows, Table).
formula_rows(lt(Col, _Val, Rows), Table):- formula_colName(Col, Table), formula_rows(Rows, Table).
formula_rows(eq(Col, _Val, Rows), Table):- formula_colName(Col, Table), formula_rows(Rows, Table).
formula_rows(max(Col, Rows),      Table):- formula_colName(Col, Table), formula_rows(Rows, Table).
formula_rows(min(Col, Rows),      Table):- formula_colName(Col, Table), formula_rows(Rows, Table).
formula_rows(prev(Rows),          Table):-                              formula_rows(Rows, Table).
formula_rows(next(Rows),          Table):-                              formula_rows(Rows, Table).

formula_values(proj(Col, Rows),   Table):- formula_colName(Col, Table), formula_rows(Rows, Table).
formula_values(L+R,               Table):- formula_values(L, Table), formula_values(R, Table).
formula_values(L-R,               Table):- formula_values(L, Table), formula_values(R, Table).
formula_values(L/R,               Table):- formula_values(L, Table), formula_values(R, Table).
formula_values(L*R,               Table):- formula_values(L, Table), formula_values(R, Table).
formula_values(card(Rows),        Table):- formula_rows(Rows, Table).
	
formula_row(first(Rows),          Table):- formula_rows(Rows, Table).

formula_value(X):- number(X).
formula_value(X):- string(X).
formula_value(Date):- (functor(Date, date, N), (N==9; N==3)); functor(Date, time, 3).
formula_value([]).
formula_value([X | Xs]):- formula_value(X), formula_value(Xs).


%% evaluating formulas:
% eval_top(Form, Table, Res):- evaluate Form with Table to produce Res, type-checking first.
% eval(Form, Table, Res):- evaluate Form with Table to produce Res.

eval_top(Form, Table, Res):- formula(Form, Table), eval(Form, Table, Res).

% Base case -- normalized indices, values.
eval(indices(L), _Table, indices(L)).
eval(val(X),     _Table, val(X)).
eval(X,          _Table, val(X)):- formula_value(X).

% 
eval(all, Table, indices(Res)):- all_rows(Table, Res).
eval(either(R,S), Table, indices(Res)):- 
	eval(R, Table, indices(IndR)),
	eval(S, Table, indices(IndS)),
	append(IndR, IndS, Ind),
	sort(Ind, Res).

eval(both(R,S), Table, indices(Res)):- 
	eval(R, Table, indices(IndR)),
	eval(S, Table, indices(IndS)),
	intersection(IndR, IndS, Res).

eval(card(Rows), Table, val(R)):-
	eval(Rows, Table, indices(Inds)),
	length(Inds, R).

eval(first(Rows), Table, indices([Row])):-
	eval(Rows, Table, indices([Row|_])).

eval(next(Rows), Table, indices(Res)):-
	eval(Rows, Table, indices(Inds)),
	next(Inds, Table, Res).

eval(prev(Rows), Table, indices(Res)):-
	eval(Rows, Table, indices(Inds)),
	prev(Inds, Res).

eval(max(ColName, Rows), Table, indices(Res)):-
	eval(Rows, Table, indices(Inds)),
	col(ColName, Table, J),
	extrema(max, J, Inds, Table, Res).

eval(min(ColName, Rows), Table, indices(Res)):-
	eval(Rows, Table, indices(Inds)),
	col(ColName, Table, J),
	extrema(min, J, Inds, Table, Res).


eval(ge(ColName, Cell, Rows), Table, indices(Res)):- comp(ge, ColName, Cell, Rows, Table, Res).
eval(gt(ColName, Cell, Rows), Table, indices(Res)):- comp(gt, ColName, Cell, Rows, Table, Res).
eval(le(ColName, Cell, Rows), Table, indices(Res)):- comp(le, ColName, Cell, Rows, Table, Res).
eval(lt(ColName, Cell, Rows), Table, indices(Res)):- comp(lt, ColName, Cell, Rows, Table, Res).
eval(eq(ColName, Cell, Rows), Table, indices(Res)):- comp(eq, ColName, Cell, Rows, Table, Res).

eval(proj(ColName, Rows), Table, Res):-
	eval(Rows, Table, indices(Inds)),
	col(ColName, Table, J),
	proj(J, Inds, Table, Res).

eval(V+W, Table, Res):- valop(plus, V, W, Table, Res).
eval(V*W, Table, Res):- valop(mult, V, W, Table, Res).
eval(V/W, Table, Res):- valop(divide, V, W, Table, Res).
eval(V-W, Table, Res):- valop(minus, V, W, Table, Res).

valop(Op, L, R, Table, Res):-
	eval(L, Table, val(L1)),
	eval(R, Table, val(R1)),
	valop(Op, L1, R1, Res).
	
comp(Op, ColName, Cell, Rows, Table, Res):-
	eval(Rows, Table, indices(Inds)),
	col(ColName, Table, J),
	Table = table(_, TRows), 
	comp_1(Op, J, Cell, Inds, TRows, Res).

% Support definitions
row(I, table(_ColNames, Rows), Row):- arg(I, Rows, Row).
col(ColName, table(ColNames, _), J):- % J is the index of ColName.
	functor(ColNames, _, K), 
	col(ColName, 1, K, ColNames, J).
col(ColName, I, K, ColNames, J):-
	arg(I, ColNames, C),
	(C == ColName
	-> J=I
	; (I < K, I1 is I+1, col(ColName, I1, K, ColNames, J))).

cell(I, J, TRows, Cell):- arg(I, TRows, Row), arg(J, Row, Cell).

range(K, K, [K]).
range(I, K, [I | Res]):- I < K, I1 is I+1, range(I1, K, Res).

all_rows(table(_, TRows), ToK):- functor(TRows, rows, K), range(1, K, ToK).

next(Inds, table(_, Rows), Res):- functor(Rows, _, K), next_1(Inds, K, Res).
next_1([], _K, []).
next_1([I|Inds], K, [I1|Res]):- I < K, I1 is I+1, next_1(Inds, K, Res).
next_1([I|Inds], K, Res)     :- I >= K,           next_1(Inds, K, Res).

prev([], []).
prev([I|Inds], [I1|Res]):- I > 1, I1 is I-1, prev(Inds, Res).
prev([I|Inds], Res)     :- I =< 1,           prev(Inds, Res).

     
comp_1(_Op, _J, _Cell, [], _TRows, []).
comp_1(Op, J, Cell, [I | Inds], TRows, Res):-
	cell(I, J, TRows, Cell1),
	comp_cell(Op, Cell1, Cell, I, Res, Res1),
	comp_1(Op, J, Cell, Inds, TRows, Res1).

comp_cell(ge, Cell1, Cell, I,  [I|Res1], Res1):- Cell1 >= Cell.
comp_cell(ge, Cell1, Cell, _I, Res,      Res) :- Cell1 < Cell.
comp_cell(gt, Cell1, Cell, I,  [I|Res1], Res1):- Cell1 > Cell.
comp_cell(gt, Cell1, Cell, _I, Res,      Res) :- Cell1 =< Cell.
comp_cell(le, Cell1, Cell, I,  [I|Res1], Res1):- Cell1 =< Cell.
comp_cell(le, Cell1, Cell, _I, Res,      Res) :- Cell1 > Cell.
comp_cell(lt, Cell1, Cell, I,  [I|Res1], Res1):- Cell1 < Cell.
comp_cell(lt, Cell1, Cell, _I, Res,      Res) :- Cell1 >= Cell.
comp_cell(eq, Cell1, Cell, I,  [I|Res1], Res1):- Cell1 == Cell.
comp_cell(eq, Cell1, Cell, _I, Res,      Res) :- Cell1 \== Cell.


extrema(Op, J, [I | Inds], table(_, TRows), Res):-
	cell(I, J, TRows, Cell), 
	extrema(Op, J, Inds, Cell-[I], TRows, Res).

extrema_cell(max, Cell1, Cell, I, _Is, Cell1-[I])   :- Cell1 > Cell.
extrema_cell(max, Cell1, Cell, I,  Is, Cell-[I|Is]) :- Cell1 == Cell.
extrema_cell(max, Cell1, Cell, _I, Is, Cell-Is)     :- Cell1 < Cell.
extrema_cell(min, Cell1, Cell, _I, Is, Cell-Is)     :- Cell1 > Cell.
extrema_cell(min, Cell1, Cell, I,  Is, Cell-[I|Is]) :- Cell1 == Cell.
extrema_cell(min, Cell1, Cell, I, _Is, Cell1-[I])   :- Cell1 < Cell.


extrema(_Op, _J, [], _Cell-Maxes, _TRows, Maxes).
extrema(Op, J, [I|Inds], Cell-Maxes, TRows, Res):-
	cell(I, J, TRows, Cell1),
	extrema_cell(Op, Cell1, Cell, I, Maxes, New),
	extreme(Op, J, Inds, New, TRows, Res).


proj(J, Inds, table(_, TRows), Res):- proj_1(J, Inds, TRows, Res).
proj_1(_J, [], _TRows, []).
proj_1(J, [I|Inds], TRows, [Cell|Res]):- cell(I, J, TRows, Cell), proj_1(J, Inds, TRows, Res).


valop(Op, L, R, Res):- is_list(L), is_list(R), valop_list(Op, L, R, Res).
valop(Op, L, R, Res):- number(L), number(R), perform_op(Op, L, R, Res).

valop_list(_Op, [], [], []).
valop_list(Op, [L|Ls], [R|Rs], [X|Xs]):-
	perform_op(Op, L, R, X),
	valop_list(Op, Ls, Rs, Xs).

perform_op(plus,   L, R, X):- X is L+R.
perform_op(minus,  L, R, X):- X is L-R.
perform_op(mult,   L, R, X):- X is L*R.
perform_op(divide, L, R, X):- X is L/R.