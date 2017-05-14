table(table([year, city, country, nations],
	    [row(1896, athens, greece, 14),
	     row(1900, paris, france, 24),
	     row(1904, 'st louis', usa, 12),
	     row(2004, athens, greece, 201),
	     row(2008, beijing, china, 204),
	     row(2012, london, uk, 204)])).
:- begin_tests(eval).
test("Greece held its last Summer Olympics in which year?"):-
	table(Table),
	eval(proj(year, max(year, eq(country, greece, all))), Table, val([2004])).

test("In which city’s the first time with at least 20 nations?"):-
	table(Table),
	eval_top(proj(city, min(year, ge(nations, 20, all))), Table, val([paris])).

test("Which years have the most participating countries?"):-
	table(Table),
	eval_top(proj(year, max(nations, all)), Table, val([2008,2012])).

test("How many events were in Athens, Greece?"):-
	table(Table),
	eval_top(card(eq(city, athens, all)), Table, val([2])).

test("How many more participants were there in 1900 than in the first year?"):-
	table(Table),
	eval_top(proj(nations, eq(year, 1990, all))-proj(nations, min(year, all)), Table, val([10])).

:- end_tests(eval).
	    