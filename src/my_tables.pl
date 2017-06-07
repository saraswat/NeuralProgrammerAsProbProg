table_1(Table):-
	table_1_data(OT),
	once(processed_table(OT, Table)).

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
	     row(1984, 'los angeles', usa, 140),
	     row(1988, seoul, korea, 159),
	     row(1992, barcelona, spain, 169),
	     row(1996, atlanta, usa, 197),
	     row(2000, sydney, australia, 199),
	     row(2004, athens, greece, 201),
	     row(2008, beijing, china, 204),
	     row(2012, london, uk, 204)])).