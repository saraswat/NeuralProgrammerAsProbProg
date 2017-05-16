/*Yeah let's try with a simple example like:
 S -> N:select_left VP:delete_left
 VP -> V:identity
 VP -> V:delete_right N:select_right
 V -> "swims", "plays", "eat"
 N -> "williams", "phelps", "tennis", "horses", "hay"

Some example derivations would be
  (S athleteplayssport(phelps,swimming)
    (N phelps "phelps")
    (VP athleteplayssport(null,swimming)
      (V athleteplayssport(null,swimming) "swims")))

  (S animaleatsfood(horses,hay)
    (N horses "horses")
    (VP animaleatsfood(null,hay)
      (V animaleatsfood(null,null) "eat")
      (N hay "hay")))
*/

s(H-T, VP):-
	n(H-M, N),   % N:selecte_right.
	vp(M-T, VP), % VP:delete_left-- why is delete_left needed?
	arg(1, VP, N).

vp(H-T, V):-
	v(H-M, V), % V:delete_right -- why is delete_right needed?
	n(M-T, N), % N:select_right
	arg(2, V, N). 
vp(H-T, V):- v(H-T, V).

n([tennis  |X]-X, tennis).
n([horses  |X]-X, horses).
n([hay     |X]-X, hay).
n([piano   |X]-X, piano).
n([mandolin|X]-X, mandolin).
n([golf    |X]-X, golf).
n([cricket |X]-X, cricket).

n(['Andre', 'Agassi'|X]-X, agassi).
n(['Williams'       |X]-X, williams).
n(['Phelps'         |X]-X, phelps).
n(['Chopin'         |X]-X, chopin).

v([swims|X]-X, 'athlete plays sport'(_, swimming)).
v([eats|X]-X,  'animal eats food'(_,_)).
v([eat|X]-X,   'animals eats food'(_,_)).
v([plays|X]-X, 'athlete plays _sport'(_,S))     :- sport(S).
v([plays|X]-X, 'musician plays instrument'(_,I)):- instrument(I).

instrument(piano).
instrument(mandolin).

sport(tennis).
sport(golf).
sport(cricket).