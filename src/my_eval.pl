/*
  ColName is of the form cols(C1, ..., CJ). 
  Rows is of the form rows(row(V11,..., V1J), ..., row(VI1,...VIJ)).

  The subsequence of rows is represented as an increasing sequence of indices in 1..J,
  indices([i1, ..., ik]). Hence, they qualify as sets, and operations such as intersection
  can be used with them. (There is no particular reason to keep them sorted, seems this
  will be of use later.)
  
*/
% /*Comment for SWIPL
/* for XSB
    :- auto_table.
 */
/* For XSB
:- table form_rows/3 as intern.
:- table form_values/3 as intern.
:- table form_value/3 as intern.
  */
%/* For BP
 :- table form_values/3, form_rows/3, form_value/3.  
%*/


% Top-level entry points:
my_top_s(D, S, N) :-
	time((table_1(T), gen_solutions(S, D, T), length(S, N))).

gen_solutions(S, Depth, T):-
	setof(X-Form, (form_values(Form, Depth, T), eval(Form, T, val(X))), S).

my_top_p(D, S, N) :-
	time((table_1(T), gen_programs(S, D, T), length(S, N))).

gen_programs(S, Depth, T):-
	bagof(Form, (form_values(Form, Depth, T)), S).


/* 
   Type checking formulas
   formula(F, T) :- F is a legal formula, given table T
*/
form(Form, Table):- form(Form, _Depth, Table).
form_values(Form, Table):- form_values(Form, _Depth, Table).

/*
 form(Form, Depth, T):-
  Form is a logical form of depth at most Depth, with colnames and constants
  specified in Table T.

  Can be used generatively.
 */

form(Form, Depth, T):- form_values(Form, Depth, T).
form(Form, Depth, T):- form_value(Form, Depth, T).
form(Form, Depth, T):- form_rows(Form, Depth, T).

/*
  Given input itable(ColNames, RowList), Types is the types of the columns,
  inferred by examining the data in Data, Numbers is the set of all numbers
  in Data, and Dates is the set of all dates in Data.
*/
processed_table(itable(ColNames, Data), table(ColNames, Types, Data, Numbers, Dates)):-
	get_types(Data, Types), 
	get_dates(Data, Dates),
	get_nums(Data, Nums-[]),
	sort(Nums, Numbers).

/*
  eval_top(Form, Table, Res):- evaluate Form with Table to produce Res, type-checking first.
  eval(Form, Table, Res):- evaluate Form with Table to produce Res.
*/
eval_top(Form, Table, Res):- once(form(Form, Table)), eval(Form, Table, Res).


% Section Table. Table is of the form table(ColNames, ColTypes, RowList, Numbers, Dates).

colNames(table(X, _, _, _, _), X).
colTypes(table(_, X, _, _, _), X).
rowList(table(_, _, X, _, _), X).
numbers(table(_, _, _, X, _), X).
dates(table(_, _, _, _, X), X).

get_types([Row | Rows], Type):- get_type(Row, T), get_types(Rows, T, Type).

get_types([], T, T).
get_types([Row | Rows], T, Types):- get_type(Row, T1), merge_types(T1, T, T2), get_types(Rows, T2, Types).

get_type(Row, Types):- Row=..[row|Args], get_type_1(Args, Types).
get_type_1([], []).
get_type_1([A|As], [T|Ts]):- type_d(A,T), get_type_1(As, Ts).

merge_types([T|Tr], [S|Sr], [U|Ur]):- merge_type(T, S, U), merge_types(Tr, Sr, Ur).
merge_types([],[],[]).

merge_type(X, Y, Z):- X == Y -> Z=X; Z=unk.

get_dates(_L, []). % TODO: extract dates from tables.
get_nums([], X-X).
get_nums([Row | Rows], X-Z):-
	Row =..[_Functor | Args],
	nums_rows(Args, X-Y),
	get_nums(Rows, Y-Z).

nums_rows([], X-X).
nums_rows([A|R], T-Y):- (number(A) -> T=[A|X];T=X), nums_rows(R, X-Y).

% Section logical forms
form_col(Col, Type, Table):- colNames(Table, Cs), colTypes(Table, Ct), member(Col, Type, Cs, Ct).

form_rows(all, D,  _T):- D >= 1.
form_rows(X,   D,   T):- dec(D, E), form_rows_1(X, E, T).
%form_rows(both(R, S),  D, T)    :- dec(D, E), form_rows(R, E, T),  form_rows(S, E, T).
form_rows_1(either(R,S),      E, T):- form_rows(R, E, T),  form_rows(S, E,  T).
form_rows_1(ge(Col, V, Rows), E, T):- form_rows(Rows, E, T), form_value(V, E, T), are_nums(Col, V, T).
form_rows_1(gt(Col, V, Rows), E, T):- form_rows(Rows, E, T), form_value(V, E, T), are_nums(Col, V, T).
form_rows_1(le(Col, V, Rows), E, T):- form_rows(Rows, E, T), form_value(V, E, T), are_nums(Col, V, T).
form_rows_1(lt(Col, V, Rows), E, T):- form_rows(Rows, E, T), form_value(V, E, T), are_nums(Col, V, T).
form_rows_1(eq(Col, V, Rows), E, T):- form_rows(Rows, E, T), form_value(V, E, T), are_same(Col, V, T).
form_rows_1(max(Col, Rows),   E, T):- is_num(Col, T),      form_rows(Rows, E, T).
form_rows_1(min(Col, Rows),   E, T):- is_num(Col, T),     form_rows(Rows, E, T).
form_rows_1(prev(Rows),       E, T):-                      form_rows(Rows, E, T).
form_rows_1(next(Rows),       E, T):-                      form_rows(Rows, E, T).
form_rows_1(first(Rows),      E, T):-                      form_rows(Rows, E, T).
form_rows_1(last(Rows),       E, T):-                      form_rows(Rows, E, T).

form_values(X, D, T) :- dec(D, E), form_values1(X, E, T).
form_values1(proj(Col, Rows),  E, T):- form_col(Col, _, T),  form_rows(Rows,E, T).
form_values1(L+R,              E, T):- form_values(L, E, T), form_values(R, E, T), are_nums_v(L, R, T).
form_values1(L-R,              E, T):- form_values(L, E, T), form_values(R, E, T), are_nums_v(L, R, T).
form_values1(L/R,              E, T):- form_values(L, E, T), form_values(R, E, T), is_num_v(L, T), is_num_v(R, T).
form_values1(L*R,              E, T):- form_values(L, E, T), form_values(R, E, T), are_nums_v(L, R, T).
form_values1(card(Rows),       E, T):-                       form_rows(Rows, E, T).

form_value(X, ignore, _T)       :- base_value(X).
form_value(X, ignore, T)        :- form_values(X, T). % maybe at runtime we will get a singleton value.
form_value(X, D, T) :- D >= 1, numbers(T, Numbers), member(X, Numbers).

base_value(X)       :- atomic(X).
base_value(Date)    :- (functor(Date, date, N), (N==9; N==3)); functor(Date, time, 3).
base_value([X | Xs]):- base_value(X), base_value(Xs).

dec(D, E):- D > 1, E is D-1.

%% type-checking
num_type(int).
num_type(float).
are_same(Col, Val, Table):- are_same(Col, Val, Table, _Type).
are_nums(Col, Val, Table):- are_same(Col, Val, Table, Type), num_type(Type).

is_num(Col,        Table):- form_col(Col, Type, Table), num_type(Type).
is_num_v(Col,      Table):- is_num_v(Col, Table, _Type).
are_nums_v(L, R,   Table):- is_num_v(L, Table, Type), is_num_v(R, Table, Type).

are_same(Col, Val, Table, Type):- form_col(Col, Type, Table), type_v(Val, Type).
is_num_v(Exp, Table, Type):- type_v(Exp, Table, Type), num_type(Type).

% type of value. it can be a constant or a +, -, *, /, card or proj.
type_v(L+_, Table, Type):- type_v(L, Table, Type).
type_v(L-_, Table, Type):- type_v(L, Table, Type).
type_v(L*_, Table, Type):- type_v(L, Table, Type).
type_v(_/_, _Table, float).
type_v(card(_), _Table, int).
type_v(proj(Col, _), Table, Type):- form_col(Col, Type, Table).
type_v(Exp, Type):- type_d(Exp, Type).

% type of datum
type_d(A, int):- integer(A).
type_d(A, float):- float(A).
type_d(A, atom):- atom(A).
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
%eval(both(R,S), Table, indices(Res)):- 
%	eval(R, Table, indices(IndR)),
%	eval(S, Table, indices(IndS)),
%	intersection(IndR, IndS, Res).

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

%row(I, table(_ColNames, Rows), Row):- arg(I, Rows, Row).
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

				% utilities.
member(X, Y, [X|_], [Y|_]).
member(X, Y, [_|Xs], [_|Ys]):- member(X, Y, Xs, Ys).

/* Comment for bp.
nth1(I, TRows, Row):- nth1(I, 1, TRows, Row).
nth1(I, I, [Row|_], Row).
nth1(I, J, [_|Rows], Row):- J < I, J1 is J + 1, nth1(I, J1, Rows, Row).

append([], X, X).
append([A|R], S, [A|T]):- append(R, S, T).

last(L, S):- append(_, [S], L).

member(X, S):- append(_, [X|_], S).
	
member_chk(X, S):- once(member(X,S)).

intersection([], _, []).
intersection([H|T], L2, Out) :-
	(member_chk(H, L2) -> Out=[H|L3]; Out=L3),
	intersection(T, L2, L3).

%/* Comment for SWIPL
length(L, X):- length(L, 0, X).
length([], A, A).
length([_|X], A, B):- A1 is A+1, length(X, A1, B).
%*/