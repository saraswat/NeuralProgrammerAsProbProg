:- ensure_loaded(eval).

table_1(Table):-
	table_1_data(OT),
	processed_table(OT, Table).

table_1_data(itable([year, city, country, nations],
	    [row(1896, athens, greece, 14),
	     row(1900, paris, france, 24),
	     row(1904, 'st louis', usa, 12),
	     row(1908, london, uk, 22),
	     row(1912, stockholm, sweden, 28),
	     row(1916, berlin, germany, 0),
	     row(1920, antwerp, belgium, 29),
	     row(1924, paris, france, 44),
	     row(1928, amsterdam, netherlands, 46),
	     row(1932, 'los angeles', usa, 37),
	     row(1936, berlin, germany, 49),
	     row(1948, london, uk, 59),
	     row(1952, helsinki, finland, 69),
	     row(1956, melbourne, australia, 72),
	     row(1960, rome, italy, 83),
	     row(1964, tokyo, japan, 93),
	     row(1968, 'mexico city', mexico, 112),
	     row(1972, munich, germany, 121),
	     row(1976, montreal, canada, 92),
	     row(1980, moscow, 'soviet union', 80),
	     row(1984, 'los anageles', usa, 140),
	     row(1988, seoul, korea, 159),
	     row(1992, barcelona, spain, 169),
	     row(1996, atlanta, usa, 197),
	     row(2000, sydney, australia, 199),
	     row(2004, athens, greece, 201),
	     row(2008, beijing, china, 204),
	     row(2012, london, uk, 204)])).
:- begin_tests(eval).
test("Show me all entries."):-
	table_1(Table),
	all_rows(Table, ToK),
	eval(all, Table, indices(ToK)).

test("Events in Athens"):- table_1(Table), eval(eq(city, athens, all), Table, indices([1,26])).
test("Events in Athens or Beijing"):- table_1(Table),
	eval(either(eq(city, athens, all), eq(city, beijing, all)), Table, indices([1,26, 27])).
test("Events in Athens before 1990"):- table_1(Table),
	eval(lt(year, 1990, eq(city, athens, all)), Table, indices([1])).
test("How many events were in Athens, Greece?"):-
	table_1(Table),
	eval(card(eq(city, athens, all)), Table, val(2)).

test("Events in the same country as Athens"):-
	table_1(Table),
	eval(eq(country, proj(country, first(eq(city,athens, all))), all), 
		 Table, indices([1,26])).

test("Greece held its last Summer Olympics in which year?"):-
	table_1(Table),
	eval(proj(year, max(year, eq(country, greece, all))), Table, val([2004])).

test("In which city’s the first time with at least 20 nations?"):-
	table_1(Table),
	eval(proj(city, min(year, ge(nations, 20, all))), Table, val([paris])).

test("Which years have the most participating countries?"):-
	table_1(Table),
	eval(proj(year, max(nations, all)), Table, val([2008,2012])).


test("How many more participants were there in 1900 than in the first year?"):-
	table_1(Table),
	eval(proj(nations, eq(year, 1900, all))-proj(nations, min(year, all)), Table, val([10])).

:- end_tests(eval).

:- begin_tests(form).
test("Form1"):-
	table_1(Table),
	setof(Form, form(Form, 1, Table), Forms),
	Forms=[all].
	
:- end_tests(form).