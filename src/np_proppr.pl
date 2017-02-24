% Sum all elements in this 1-d array.
sum(Array, Ans):- make_array(IMax, Array), sum(1, IMax, Array, Ans).

% Ans = sum of elements in 1-d array from I to J (inclusive).
sum(I, J, Array, Ans):- sum(I, J, Array, 0.0, Ans).
sum(I, J, Array, X, Ans):-
	array(Array, I, V),
	X1 is X+V,
	(I==J -> Ans=X1 ; (I1 is I+1, sum(I1, J, Array, X1, Ans))).

make_array(I, A):- functor(A, row, I).
make_array(I, J, A):- functor(A, rows, I), cols(A, 1, I, J).
cols(A, K, I, J):- 
	arg(K, A, AK),
	functor(AK, row, J),
	(K==I ; (K < I, K1 is K+1, cols(A, K1, I, J))).

% Accessor -- X = A(I,J), X=A(I)
array(A, I, J, X) :- arg(I, A, Row), arg(J, Row, X).
array(A, I, X) :- arg(I, A, X).

assign(RS, IndexI, IndexJ, Val, Def, M, C, I, J):-
       ((I==IndexI, J==IndexJ) -> Value = Val; Value = Def),
       array(RS, I, J, Value),
       (J==C,
	 (I==M;
	   (I <M, I1 is I+1, assign(RS, IndexI, IndexJ, Val, Def, M, C, I1, 1)))
       ; (J < C, J1 is J+1, assign(RS, IndexI, IndexJ, Val, Def, M, C, I, J1))).

% Ensure RS(I..M)[I]=Val if I=Index, and Def ow
assign(RS, Index, Val, Def, I, M):-
       (I==Index -> Value = Val; Value = Def),
       array(RS, I, Value),
       (I==M; (I <M, I1 is I+1, assign(RS, Index, Val, Def, I1, M))).

assign(RS, Def, I, M) :-
	array(RS, I, Def),
	(I==M; (I < M, I1 is I+1, assign(RS, Def, I1, M))).

gt(Col, Pivot, Table, IMax, RS, I, RSOut) :-
	array(Table, I, Col, AIJ),
	(AIJ > Pivot -> Val=1; Val=0),
	array(RSOut, I, Val),
	(I==IMax ; (I < IMax, I1 is I+1, gt(Col, Pivot, IMax, RS, I1, RSOut))).

geq(Col, Pivot, Table, IMax, RS, I, RSOut) :-
	array(Table, I, Col, AIJ),
	(AIJ >= Pivot -> Val=1; Val=0),
	array(RSOut, I, Val),
	(I == IMax; (I<IMax, I1 is I+1, geq(Col, Pivot, IMax, RS, I1, RSOut))).

lt(Col, Pivot, Table, IMax, RS, I, RSOut) :-
	array(Table, I, Col, AIJ),
	(AIJ < Pivot -> Val=1; Val=0),
	array(RSOut, I, Val),
	(I==IMax; (I < IMax, I1 is I+1, lt(Col, Pivot, IMax, RS, I1, RSOut))).

leq(Col, Pivot, Table, IMax, RS, I, RSOut) :-
	array(Table, I, Col, AIJ),
	(AIJ =< Pivot -> Val=1; Val=0),
	array(RSOut, I, Val),
	(I==IMax; (I < IMax, I1 is I+1, leq(Col, Pivot, IMax, RS, I1, RSOut))).


argmax(Col, Table, M, RS, RSOut) :-
	array(Table, 1, Col, Val), 
	argmax(Col, RS, Table, M, 2, 1, Val, Index),
	assign(RSOut, Index, 1, 0, 1, M).

argmax(Col, RS, Table, M, I, CI, CVal, Index) :-
	array(RS, I, RSI), array(Table, I, Col, TIJ), 
	((RSI==1, TIJ > CVal)-> (NCI=I, NCVal=TIJ); (NCI=CI, NCVal=CVal)),
	(I = M
	  -> Index=NCI
	   ; (I1 is I+1, argmax(Col, RS, Table, M, I1, NCI, NCVal, Index))).
	
argmin(Col, Table, M, RS, RSOut) :-
	array(Table, 1, Col, Val), 
	argmin(Col, RS, Table, M, 2, 1, Val, Index),
	assign(RSOut, Index, 1, 0, 1, M).

argmin(Col, RS, Table, M, I, CI, CVal, Index) :-
	array(RS, I, RSI), array(Table, I, Col, TIJ), 
	((RSI==1, TIJ < CVal)-> (NCI=I, NCVal=TIJ); (NCI=CI, NCVal=CVal)),
	(I = M
	  -> Index=NCI
	   ; (I1 is I+1, argmin(Col, RS, Table, M, I1, NCI, NCVal, Index))).

first(_RS, M, I, 1, RSOut) :- assign(RSOut, I, 1, 0, 1, M).
first(RS,  M, I, 0, RSOut):-
	I < M, 
	array(RS, I, RSI),
	I1 is I+1, first(RS, M, I1, RSI, RSOut).
first(_RS, M, M, 0, RSOut):- assign(RSOut, 0, 1, M).

last(_RS, M, I, 1, RSOut) :- assign(RSOut, I, 1, 0, 1, M).
last(RS, M, I, 0, RSOut):-
	I > 1, 
	array(RS, I, RSI),
	I1 is I-1, last(RS, M, I1, RSI, RSOut).
last(_RS, M, 1, 0, RSOut):- assign(RSOut, 0, 1, M).

previous(RS, M, I, V, RSOut) :-
	array(RSOut, I, V),
	I1 is I+1,
	(I1=M -> array(RSOut, M, 0)
	  ; (array(RS, I1, V1), previous(RS, M, I1, V1, RSOut))).

next(RS, M, I, V, RSOut) :-
	array(RSOut, I, V), array(RS, I, V1),
	I1 is I+1,
	(I1=M; (I1 < M, next(RS, M, I1, V1, RSOut))).

print(RS, M, C, Col, LookupA) :-
	make_array(M, C, LookupA),
	print_ij(RS, M, C, Col, 1, 1, LookupA).

print_ij(RS, M, C, Col, I, J, LookupA) :-
	(J==Col -> array(RS, I, Val) ; Val=0),
	array(LookupA, I, J, Val),
	(J==C
	  -> (I == M ; (I < M, I1=I+1, print_ij(RS, M, C, I1, 1, LookupA)))
	  ; (J1 is J+1, print_ij(RS, M, C, I, J1, LookupA))).
	
select(Select, Col, M, I, RSOut) :-
	array(Select, I, Col, V),
	array(RSOut, I, V),
	(I==M; (I < M, I1 is I+1, select(Select, Col, M, I1, RSOut))).

step(RS, M, C, Table, Op, Col, S, RSOut, Ans):-
	make_array(M, RSOut),
	jump(RS, M, C, Table, Op, Col, S, RSOut, Ans).

jump(RS, M, _C, Table, gt, Col,  s(_, _, Pivot, _, _, _), RSOut, _):- 
	gt(Col, Pivot, Table, M, RS, 1, RSOut).
jump(RS, M, _C, Table, lt, Col,  s(_, _, _, Pivot, _, _), RSOut, _):- 
	lt(Col, Pivot, Table, M, RS, 1, RSOut).
jump(RS, M, _C, Table, geq, Col,  s(_, _, _, _, Pivot, _), RSOut, _):- 
	geq(Col, Pivot, Table, M, RS, 1, RSOut).
jump(RS, M, _C, Table, leq, Col,  s(_, _, _, _, _,Pivot), RSOut, _):- 
	leq(Col, Pivot, Table, M, RS, 1, RSOut).

jump(RS, M, C, Table, argmax, Col,  _, RSOut, _):- 
	argmax(Col, Table, M, C, RS, RSOut).
jump(RS, M, C, Table, argmin, Col,  _, RSOut, _):- 
	argmin(Col, Table, M, C, RS, RSOut).
jump(RS, M, _C, _Table, first, _Col,  _, RSOut, _):-
	array(RS, 1, RS1), first(RS, M, 1, RS1, RSOut).
jump(RS, M, _C, _Table, last, _Col,  _, RSOut, _):-
	array(RS, M, RSM), last(RS, M, M, RSM, RSOut).
jump(RS, M, _C, _Table, previous, _Col,  _, RSOut, _):-
	array(RS, 2, RSM), previous(RS, M, 1, RSM, RSOut).
jump(RS, M, _C, _Table, next, _Col,  _, RSOut, _):-
	next(RS, M, 1, 0, RSOut).

jump(_RS, M, _C, _Table, select, Col, s(Select,_,_,_,_,_), RSOut, _):-
	select(Select, Col, M, 1, RSOut).

jump(_RS, M, _C, _Table, select, Col, s(_,MFE,_,_,_,_), RSOut, _):-
	select(MFE, Col, M, 1, RSOut).

jump(RS, _M, _C, _Table, count, _Col, _, _, ans(ScalarA, _)):- sum(RS, ScalarA).
jump(RS, _M, _C, _Table, count, Col, _, _, ans(_,LookupA)):- print(RS, Col, LookupA).
jump(_RS, M, _C, _Table, reset, _Col, _, RSOut, _):- assign(RSOut, 1, 1, M).
     

compute_state(_Table, s(_Select, _MFE)):-
	true. % TODO

% TODO: Figure out how the Input utterance is to be represented.
% cf [Neelakantan 2017]'s Question RNN.
program(_Input, Table, Ans) :-
	compute_state(Table, InitState), 
	make_array(M, C, Table),
	make_array(M, RS0),
	assign(RS0, 1, 1, M),  

	op(Op1),
	col(Col1, C),
	step(RS0, 1, M, C, Table, Op1, Col1, InitState, RS1, _Ans1),

	op(Op2),
	col(Col2, C),
	step(RS1, 1, M, C, Table, Op2, Col2, InitState, RS2, _Ans2),

	op(Op3),
	col(Col3, C),
	step(RS2, 1, M, C, Table, Op3, Col3, InitState, RS3, _Ans3),

	op(Op4),
	col(Col4, C),
	step(RS3, 1, M, C, Table, Op4, Col4, InitState, _RS4, Ans).
	
% A probabilistic predicate -- op selector.
% We have to learn the probabilities associated with each clause.

op(gt)      . %:- {op(gt)}.
op(lt)      . %:- {op(lt)}.
op(geq)     . %:- {op(geq)}.
op(leq)     . %:- {op(leq)}.
op(argmax)  . %:- {op(argmax)}.
op(argmin)  . %:- {op(argmin)}.
op(select)  . %:- {op(select)}.
op(mfe)     . %:- {op(mfe)}.
op(previous). %:- {op(previous)}.
op(next)    . %:- {op(next)}.
op(first)   . %:- {op(first)}.
op(last)    . %:- {op(last)}.
op(reset)   . %:- {op(reset)}.
op(print)   . %:- {op(print)}.
op(count)   . %:- {op(count)}.

% A probabilistic predicate -- column selector.
col(C, Bound):- C < Bound. %, {col(C)}.

% The paper also deals with 