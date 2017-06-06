/*
  ColName is of the form cols(C1, ..., CJ). 
  Rows is of the form rows(row(V11,..., V1J), ..., row(VI1,...VIJ)).

  The subsequence of rows is represented as an increasing sequence of indices in 1..J,
  indices([i1, ..., ik]). Hence, they qualify as sets, and operations such as intersection
  can be used with them. (There is no particular reason to keep them sorted, seems this
  will be of use later.)
  
*/

:- use_module(library(lists)).

% Top-level entry points:
go :- time((table_1(T), gen_solutions(S, 4, T, N))),
	print(solutions(S)),
	print(time(N)).
gen_solutions(S, Depth, T, N):-
	setof(sol(Form, X), (form_values(Form, Depth, T), eval(Form, T, val(X))), S), length(S, N).

%% Type checking formulas
% formula(F, T) holds if F is a legal formula, given table T
form(Form, Table):- form(Form, _Depth, Table).

% form(Form, Depth, T):-
%  Form is a logical form of depth at most Depth, with colnames and constants
% specified in Table T.

form(Form, Depth, T):- form_values(Form, Depth, T).
form(Form, Depth, T):- form_value(Form, Depth, T).
form(Form, Depth, T):- form_rows(Form, Depth, T).

processed_table(itable(ColNames, Data), table(ColNames, Data, Numbers, Dates)):-
	get_dates(Data, Dates),
	get_nums(Data, Nums-[]),
	list_to_set(Nums, Numbers).

% eval_top(Form, Table, Res):- evaluate Form with Table to produce Res, type-checking first.
% eval(Form, Table, Res):- evaluate Form with Table to produce Res.

eval_top(Form, Table, Res):- once(form(Form, Table)), eval(Form, Table, Res).


% Table is a processed table, of the form table(ColNames, RowList, Numbers, Dates).

colNames(table(X, _, _, _), X).
rowList(table(_, X, _, _), X).
numbers(table(_, _, X, _), X).
dates(table(_, _, _, X), X).

get_dates(_L, []). % TODO: extract dates from tables.
get_nums([], X-X).
get_nums([Row | Rows], X-Z):-
	Row =..[_Functor | Args],
	nums_rows(Args, X-Y),
	get_nums(Rows, Y-Z).

nums_rows([], X-X).
nums_rows([A|R], [A|X]-Y):- number(A), nums_rows(R, X-Y).
nums_rows([A|R], X-Y)    :- \+ number(A), nums_rows(R, X-Y).

form_col(Col, Table):- colNames(Table, Cs), member(Col, Cs).

form_rows(all,         D,  _T):- D >= 1.
form_rows(either(R,S), D, T)    :- dec(D, E), form_rows(R, E, T),  form_rows(S, E,  T).
form_rows(both(R, S),  D, T)    :- dec(D, E), form_rows(R, E, T),  form_rows(S, E, T).
form_rows(ge(Col, Val, Rows), D, T):- dec(D, E), form_col(Col, T), form_rows(Rows, E, T), form_value(Val, E, T).
form_rows(gt(Col, Val, Rows), D, T):- dec(D, E), form_col(Col, T), form_rows(Rows, E, T), form_value(Val, E, T).
form_rows(le(Col, Val, Rows), D, T):- dec(D, E), form_col(Col, T), form_rows(Rows, E, T), form_value(Val, E, T).
form_rows(lt(Col, Val, Rows), D, T):- dec(D, E), form_col(Col, T), form_rows(Rows, E, T), form_value(Val, E, T).
form_rows(eq(Col, Val, Rows), D, T):- dec(D, E), form_col(Col, T), form_rows(Rows, E, T), form_value(Val, E, T).
form_rows(max(Col, Rows),     D, T):- dec(D, E), form_col(Col, T), form_rows(Rows, E, T).
form_rows(min(Col, Rows),     D, T):- dec(D, E), form_col(Col, T), form_rows(Rows, E, T).
form_rows(prev(Rows),         D, T):- dec(D, E),                   form_rows(Rows, E, T).
form_rows(next(Rows),         D, T):- dec(D, E),                   form_rows(Rows, E, T).
form_rows(first(Rows),        D, T):- dec(D, E),                   form_rows(Rows, E, T).
form_rows(last(Rows),         D, T):- dec(D, E),                   form_rows(Rows, E, T).

form_values(proj(Col, Rows),  D, T):- dec(D, E), form_col(Col, T),     form_rows(Rows, E, T).
form_values(L+R,              D, T):- dec(D, E), form_values(L, E, T), form_values(R, E, T).
form_values(L-R,              D, T):- dec(D, E), form_values(L, E, T), form_values(R, E, T).
form_values(L/R,              D, T):- dec(D, E), form_values(L, E, T), form_values(R, E, T).
form_values(L*R,              D, T):- dec(D, E), form_values(L, E, T), form_values(R, E, T).
form_values(card(Rows),       D, T):- dec(D, E),                       form_rows(Rows, E, T).

form_value(X, ignore, _T)       :- base_value(X).
form_value(X, ignore, T)        :- form_values(X, T). % maybe at runtime we will get a singleton value.

form_value(X, N, Table) :- N >= 1, numbers(Table, Numbers), member(X, Numbers).

base_value(X)       :- atomic(X).
base_value(Date)    :- (functor(Date, date, N), (N==9; N==3)); functor(Date, time, 3).
base_value([X | Xs]):- form_value(X), form_value(Xs).

dec(D, E):- D > 1, E is D-1.
	
%% evaluating forms:

% Base case -- normalized indices, values.
eval(indices(L), _Table, indices(L)).
eval(val(X),     _Table, val(X)).
eval(X,          _Table, val(X)):- X \== all, base_value(X).

% 
eval(all, Table, indices(Res)):- all_rows(Table, Res).
eval(either(R,S), Table, indices(Res)):- 
	eval(R, Table, indices(IndR)),
	eval(S, Table, indices(IndS)),
	append(IndR, IndS, Ind),
	sort(Ind, Res).

% probably both is not needed. One can do a CPS embedding of R in S.
eval(both(R,S), Table, indices(Res)):- 
	eval(R, Table, indices(IndR)),
	eval(S, Table, indices(IndS)),
	intersection(IndR, IndS, Res).

eval(card(Rows),  Table, val(R))        :- eval(Rows, Table, indices(Inds)), length(Inds, R).
eval(first(Rows), Table, indices([Row])):- eval(Rows, Table, indices([Row|_])).
eval(last(Rows),  Table, indices([Row])):- eval(Rows, Table, indices(Ind)),  last(Ind, Row).
eval(next(Rows),  Table, indices(Res))  :- eval(Rows, Table, indices(Inds)), next(Inds, Table, Res).
eval(prev(Rows),  Table, indices(Res))  :- eval(Rows, Table, indices(Inds)), prev(Inds, Res).

eval(max(Col, Rows), Table, indices(Res)):- e_extrema(max, Col, Rows, Table, Res).
eval(min(Col, Rows), Table, indices(Res)):- e_extrema(min, Col, Rows, Table, Res).

eval(ge(ColName, Cell, Rows), Table, indices(Res)):- e_comp(ge, ColName, Cell, Rows, Table, Res).
eval(gt(ColName, Cell, Rows), Table, indices(Res)):- e_comp(gt, ColName, Cell, Rows, Table, Res).
eval(le(ColName, Cell, Rows), Table, indices(Res)):- e_comp(le, ColName, Cell, Rows, Table, Res).
eval(lt(ColName, Cell, Rows), Table, indices(Res)):- e_comp(lt, ColName, Cell, Rows, Table, Res).
eval(eq(ColName, Cell, Rows), Table, indices(Res)):- e_comp(eq, ColName, Cell, Rows, Table, Res).

eval(proj(ColName, Rows), Table, val(Res)):-
	eval(Rows, Table, indices(Inds)),
	col(ColName, Table, J),
	proj(J, Inds, Table, Res).

eval(V+W, Table, val(Res)):- e_valop(plus, V, W, Table, Res).
eval(V*W, Table, val(Res)):- e_valop(mult, V, W, Table, Res).
eval(V/W, Table, val(Res)):- e_valop(divide, V, W, Table, Res).
eval(V-W, Table, val(Res)):- e_valop(minus, V, W, Table, Res).

e_extrema(Op, ColName, Rows, Table, Res):-
	eval(Rows, Table, indices(Inds)),
	col(ColName, Table, J),
	extrema(Op, J, Inds, Table, Res).

e_valop(Op, L, R, Table, Res):-
	eval(L, Table, val(L1)),
	eval(R, Table, val(R1)),
	valop(Op, L1, R1, Res).
	
e_comp(Op, ColName, CellForm, Rows, Table, Res):-
	eval(CellForm, Table, Vals), single_value(Vals, Cell), 
	eval(Rows, Table, indices(Inds)),
	col(ColName, Table, J),
	rowList(Table, TRows), 
	comp_1(Op, J, Cell, Inds, TRows, Res).

				% Support definitions
single_value(val([X]), X).
single_value(val(X), X):- atomic(X).

row(I, table(_ColNames, Rows), Row):- arg(I, Rows, Row).
col(ColName, Table, J)  :- colNames(Table, ColNames),    col(ColName, 1, ColNames, J).
col(ColName, I, [ColName | _Rest], I).
col(ColName, I, [C | Rest], J)       :- ColName \== C, I1 is I+1, col(ColName, I1, Rest, J).

cell(I, J, TRows, Cell):- nth1(I, TRows, Row), arg(J, Row, Cell).

range(K, K, [K]).
range(I, K, [I | Res]):- I < K, I1 is I+1, range(I1, K, Res).

all_rows(Table, ToK):- rowList(Table, TRows), length(TRows, K), once(range(1, K, ToK)).

next(Inds, Table, Res):- rowList(Table, TRows), length(TRows, K), next_1(Inds, K, Res).
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

comp_cell(ge, Cell1, Cell, I,  [I|Res1], Res1):- number(Cell1), number(Cell), Cell1 >= Cell.
comp_cell(gt, Cell1, Cell, I,  [I|Res1], Res1):- number(Cell1), number(Cell), Cell1 > Cell.
comp_cell(le, Cell1, Cell, I,  [I|Res1], Res1):- number(Cell1), number(Cell), Cell1 =< Cell.
comp_cell(lt, Cell1, Cell, I,  [I|Res1], Res1):- number(Cell1), number(Cell), Cell1 < Cell.
comp_cell(eq, Cell1, Cell, I,  [I|Res1], Res1):- Cell1 == Cell.
comp_cell(ge, Cell1, Cell, _I, Res,      Res) :- number(Cell1), number(Cell), Cell1 < Cell.
comp_cell(gt, Cell1, Cell, _I, Res,      Res) :- number(Cell1), number(Cell), Cell1 =< Cell.
comp_cell(le, Cell1, Cell, _I, Res,      Res) :- number(Cell1), number(Cell), Cell1 > Cell.
comp_cell(lt, Cell1, Cell, _I, Res,      Res) :- number(Cell1), number(Cell), Cell1 >= Cell.
comp_cell(eq, Cell1, Cell, _I, Res,      Res) :- Cell1 \== Cell.


extrema(Op, J, [I | Inds], Table, Res):-
	rowList(Table, TRows), 
	cell(I, J, TRows, Cell), 
	extrema(Op, J, Inds, Cell-[I|Tail]-Tail, TRows, Res).

extrema_cell(max, Cell1,  I, Cell-_Is-_Tail,      Cell1-[I|NewTail]-NewTail) :-
	number(Cell), number(Cell1), Cell1 > Cell.
extrema_cell(max, Cell1,  I, Cell-Is-[I|NewTail], Cell-Is-NewTail)           :-
	number(Cell), number(Cell1), Cell1 == Cell.
extrema_cell(max, Cell1, _I, Cell-Is-Tail,        Cell-Is-Tail)              :-
	number(Cell), number(Cell1), Cell1 < Cell.
extrema_cell(min, Cell1, _I, Cell-Is-Tail,        Cell-Is-Tail)              :-
	number(Cell), number(Cell1), Cell1 > Cell.
extrema_cell(min, Cell1,  I, Cell-Is-[I|NewTail], Cell-Is-NewTail)           :-
	number(Cell), number(Cell1), Cell1 == Cell.
extrema_cell(min, Cell1,  I, Cell-_Is-_Tail,      Cell1-[I|NewTail]-NewTail) :-
	number(Cell), number(Cell1), Cell1 < Cell.


extrema(_Op, _J, [], _Cell-Maxes-[], _TRows, Maxes).
extrema(Op, J, [I|Inds], Old, TRows, Res):-
	cell(I, J, TRows, Cell1),
	extrema_cell(Op, Cell1, I, Old, New),
	extrema(Op, J, Inds, New, TRows, Res).


proj(J, Inds, Table, Res):- rowList(Table, TRows), proj_1(J, Inds, TRows, Res).
proj_1(_J, [], _TRows, []).
proj_1(J, [I|Inds], TRows, [Cell|Res]):- cell(I, J, TRows, Cell), proj_1(J, Inds, TRows, Res).


valop(Op, L, R, Res):- is_list(L), is_list(R), valop_list(Op, L, R, Res).
valop(Op, L, R, Res):- number(L), number(R), perform_op(Op, L, R, Res).

valop_list(_Op, [], [], []).
valop_list(Op, [L|Ls], [R|Rs], [X|Xs]):-
	perform_op(Op, L, R, X),
	valop_list(Op, Ls, Rs, Xs).

perform_op(plus,   L, R, X):- number(L), number(R), X is L+R.
perform_op(minus,  L, R, X):- number(L), number(R), X is L-R.
perform_op(mult,   L, R, X):- number(L), number(R),  X is L*R.
perform_op(divide, L, R, X):- number(L), number(R),  R \==0, X is L/R.