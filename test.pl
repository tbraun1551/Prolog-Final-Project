problem4(E) :-

	A = [B],
    A::0..99999,
    member(E, A),

    E #= (E2 * 2) + 1,
    E #= (E3 * 3) + 1,
    E #= (E4 * 4) + 1,
    E #= (E5 * 5) + 1,
    E #= (E6 * 6) + 1,
    E #= (E7 * 7),
    
    B = E,
    
    minimize(labeling(A), B).



%Q2

beer(Beer):-
	Quantities = [15, 31, 19, 20, 16, 18],
	Barrels = [A, B, C, D, E, Beer],
	Barrels :: 15..31,
	Sale1 #= A + B,
	Sale2 #= C + D + E,
	Sale2 #= Sale1 * 2,
	member(A, Quantities),
	member(B, Quantities),
	member(C, Quantities),
	member(D, Quantities),
	member(E, Quantities),
	member(Beer, Quantities),
	alldifferent(Barrels).
		

question2(Barrels) :-
	Quantities = [15, 31, 19, 20, 16, 18],
	Barrels = [Beer, W1, W2, W3, W4, W5],
	Barrels :: 15..31,
	W3 + W4 + W5 #= W1 + W2 + W1 + W2,
	member(Beer, Quantities),
	alldifferent(Barrels),
	labeling(Barrels),
	printf('%3d', [Beer]).