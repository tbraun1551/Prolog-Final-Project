/*Thomas Braun, Sindel Donaldson. Odd questions*/

:- lib(lists).
:- lib(fd).
:- lib(fd_search).

%Question 1
question1(Teas) :-
	Teas = [Good, Inferior, Indian],
	Teas :: 0..20,
	Good + Inferior + Indian #= 20,

	(Good * 30) + (Inferior * 27) + (Indian * 21) #= 570,
	minimize(labeling(Teas), Good).


question2(Barrels) :-
	Quantities = [31, 19, 20, 16, 18, 15],
	Barrels = [Beer, W1, W2, W3, W4, W5],
	Barrels :: 15..31,
	W3 + W4 + W5 #= W1 + W2 + W2 + W1,
	member(Beer, Quantities),
    member(W1, Quantities),
    member(W2, Quantities),
    member(W3, Quantities),
    member(W4, Quantities),
    member(W5, Quantities),
	alldifferent(Barrels),
	labeling(Barrels),
	printf('%3d', [Beer]).

/*Question 3*/
question3(Digits) :- allDig(Digits), printDigits(Digits).
dig(Digits):-
    Digits = [A,B,C,D,E,F,G,H,I],
    Digits:: 1..9,
    alldifferent(Digits),
    ((100 * A) + (10 * B) + C) * ((10 * D) + E) #= P,
    ((10 * F) + G) * ((10 * H) + I) #= P,
    Product #= -P,
    
    minimize(labeling(Digits), Product).
	/*maximize(labeling(Digits), P). */
    
allDig(Digits):- 
    Digits = [A,B,C,D,E,F,G,H,I],
    Digits:: 1..9,
    alldifferent(Digits),
    ((100 * A) + (10 * B) + C) * ((10 * D) + E) #= P,
    ((10 * F) + G) * ((10 * H) + I) #= P,
    
    labeling(Digits).

printDigits(Digits) :-
	Digits = [A,B,C,D,E,F,G,H,I],
	printf("Num1: %d%d%d\n", [A, B, C]), 
	printf("Num2: %d%d  \n", [ D,  E ]),  
	printf("Num3: %d%d   \n", [ F,  G ]),
	printf("Num4: %d%d   \n", [ H,  I ]).


%Question 4
question4(E) :-
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


%Question 5

question5(Trusses):-
    Weights = [110, 112, 113, 114, 115, 116, 117, 118, 120, 121],
    Trusses = [A, B, C, D, E],
    Trusses::0..121,
    Tweights = [W1, W2, W3, W4, W5, W6, W7, W8, W9, W10],
    A + B #= W1, member(W1, Weights),
    A + C #= W2, member(W2, Weights),
    A + D #= W3, member(W3, Weights),
    A + E #= W4, member(W4, Weights),
    B + C #= W5, member(W5, Weights),
    B + D #= W6, member(W6, Weights),
    B + E #= W7, member(W7, Weights),
    C + D #= W8, member(W8, Weights),
    C + E #= W9, member(W9, Weights),
    D + E #= W10, member(W10, Weights),
    
    alldifferent(Tweights),
    
    labeling(Trusses).



%Question 6
question6(Alpha) :-
    Alpha = [P, L, U, T, O, R, A, N, S, E, J, I, M, H, V, C, Y],
    Alpha :: 0 .. 100,

    P + L + U + T + O #= 40,
    U + R + A + N + U + S #= 36,
    N + E + P + T + U + N + E #= 29,
    S + A + T + U + R + N #= 33,
    J + U + P + I + T + E + R #= 50,
    M + A + R + S #= 32,
    E + A + R + T + H #= 31,
    M + O + O + N #= 36,
    V + E + N + U + S #= 39,
    M + E + R + C + U + R + Y #= 33,
    S + U + N #= 18,

    alldifferent(Alpha),
    labeling(Alpha),

    PLANETS #= P + L + A + N + E + T + S,

    printf("%3d", [PLANETS]).

/*Question7*/
question7(Sons) :-
	S1 = [S11, S17, S15, S12, S10],
	S2 = [S21, S27, S25, S22, S20],
	S3 = [S31, S37, S35, S32, S30],
	S4 = [S41, S47, S45, S42, S40],
	S5 = [S51, S57, S55, S52, S50],


	S11 + S21 + S31 + S41 + S51 #= 9,
	S17 + S27 + S37 + S47 + S57 #= 9,
	S15 + S25 + S35 + S45 + S55 #= 9,
	S12 + S22 + S32 + S42 + S52 #= 9,
	S10 + S20 + S30 + S40 + S50 #= 9,

	S11 + S17 + S15 + S12 + S10 #= 9,
	S21 + S27 + S25 + S22 + S20 #= 9,
	S31 + S37 + S35 + S32 + S30 #= 9,
	S41 + S47 + S45 + S42 + S40 #= 9,
	S51 + S57 + S55 + S52 + S50 #= 9,

	S11 #>= 1, S17 #>= 1, S15 #>= 1, S12 #>= 1, S10 #>= 1,
	S21 #>= 1, S27 #>= 1, S25 #>= 1, S22 #>= 1, S20 #>= 1,
	S31 #>= 1, S37 #>= 1, S35 #>= 1, S32 #>= 1, S30 #>= 1,
	S41 #>= 1, S47 #>= 1, S45 #>= 1, S42 #>= 1, S40 #>= 1,
	S51 #>= 1, S57 #>= 1, S55 #>= 1, S52 #>= 1, S50 #>= 1,

	S1T = (S11 * 10000) + (S17 * 1000) + (S15 * 100) + (S12 * 10) + S10,
	S2T = (S21 * 10000) + (S27 * 1000) + (S25 * 100) + (S22 * 10) + S20,
	S3T = (S31 * 10000) + (S37 * 1000) + (S35 * 100) + (S32 * 10) + S30,
	S4T = (S41 * 10000) + (S47 * 1000) + (S45 * 100) + (S42 * 10) + S40,
	S5T = (S51 * 10000) + (S57 * 1000) + (S55 * 100) + (S52 * 10) + S50,

	S1T ## S2T, S1T ## S3T, S1T ## S4T, S1T ## S5T,
	S2T ## S3T, S2T ## S4T, S2T ## S5T,
	S3T ## S4T, S3T ## S5T,
	S4T ## S5T,

	F = 100, Tq = 75, Hf = 50, Oq = 25, E = 0,

	(S11 * F) + (S17 * Tq) + (S15 * Hf) + (S12 * Oq) #= W,
	(S21 * F) + (S27 * Tq) + (S25 * Hf) + (S22 * Oq) #= W,
	(S31 * F) + (S37 * Tq) + (S35 * Hf) + (S32 * Oq) #= W,
	(S41 * F) + (S47 * Tq) + (S45 * Hf) + (S42 * Oq) #= W,
	(S51 * F) + (S57 * Tq) + (S55 * Hf) + (S52 * Oq) #= W,

	labeling(S1),
	labeling(S2),
	labeling(S3),
	labeling(S4),
	labeling(S5),

	Sons = [S1, S2, S3, S4, S5].


question9(Squares) :-
    
    Squares = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P],
    Squares::0..1,
    
    LineA #= (_ * 2),
    LineA #= A + B + C + D + E + I + M,
    
    LineB #= (_ * 2),
    LineB #= B + A + C + D + F + J + N,
    
    LineC #= (_ * 2),
    LineC #= C + A + B + D + G + K + O,
    
    LineD #= (_ * 2) + 1,
    LineD #= D + A + B + C + H + L + P,
    
    LineE #= (_ * 2),
    LineE #= E + F + G + H + A + I + M,
    
    LineF #= (_ * 2),
    LineF #= F + E + G + H + B + J + N,
    
    LineG #= (_ * 2),
    LineG #= G + E + F + H + C + K + O,
    
    LineH #= (_ * 2) + 1,
    LineH #= H + E + F + G + D + L + P,
    
    LineI #= (_ * 2) + 1,
    LineI #= I + J + K + L + A + E + M,
    
    LineJ #= (_ * 2),
    LineJ #= J + I + K + L + B + F + N,
    
    LineK #= (_ * 2) + 1,
    LineK #= K + I + J + L + C + G + O,
    
    LineL #= (_ * 2),
    LineL #= L + I + J + K + G + H + P,
    
    LineM #= (_ * 2) + 1,
    LineM #= M + N + O + P + A + E + I,
    
    LineN #= (_ * 2),
    LineN #= N + M + O + P + B + F + J,
    
    LineO #= (_ * 2),
    LineO #= O + M + N + P + C + G + K,
    
    LineP #= (_ * 2),
    LineP #= P + M + N + O + D + H + L,
    
    labeling(Squares).



    %Question number 10 Party list
    men([tom, fred, billy, tim, frank, barry]).
women([sue, jane, betty, ellen, joan, betsy]).

funny([tom, sue, tim, ellen]).
interesting([fred, betty, frank, betsy]).


available(tom, saturday).
available(fred, saturday).
available(sue, saturday).
available(jane, saturday).
available(tom, friday).
available(billy, friday).
available(sue, friday).
available(betty, friday).
available(tim, saturday).
available(frank, saturday).
available(ellen, saturday).
available(joan, saturday).
available(tim, friday).
available(barry, friday).
available(ellen, friday).
available(betsy, friday).

democrat(tom).
democrat(sue).
democrat(tim).
democrat(ellen).

republican(fred).
republican(jane).
republican(frank).
republican(joan).

independent(billy).
independent(betty).
independent(barry).
independent(betsy).

know(tom, fred).
know(fred, tom).
know(fred, billy). 
know(billy, fred).
know(billy, betty). know(betty, billy).
know(betty, sue). know(tom, sue).
know(sue, tom). know(sue, betty).
know(sue, jane). know(jane, sue).
know(tim, frank). know(frank, tim).
know(frank, barry). know(barry, frank).
know(barry, betsy). know(betsy, barry).
know(betsy, ellen). know(tim, ellen).
know(ellen, tim). know(ellen, betsy).
know(ellen, joan). know(joan, ellen).

dislike(billy, fred).
dislike(sue, jane).
dislike(barry, frank).
dislike(ellen, joan).


%1
% Length included at main call of guest list

%2 Male/or Female
gender([]).
gender([G|Gs]) :- men(M), member(G,M), gender(Gs).
gender([G|Gs]) :- women(W), member(G,W), gender(Gs).


%3 Must be available
availability(Gs) :- availability1(friday, Gs).
availability(Gs) :- availability1(saturday, Gs).

availability1(_,[]).
availability1(Y, [G|Gs]) :- available(G, Y), availability1(Y, Gs).

%6 Equal number of men and women

equalGender(Gs) :- eG(0,Gs).

eG(N,[]) :- eval(N,Nr), Nr == 0.
eG(N,[G|Gs]) :- men(M), member(G,M), eG(N+1,Gs).
eG(N,[G|Gs]) :- women(W), member(G,W), eG(N-1,Gs).

%4 Must all be interesting

interestingP(Gs) :- not unInteresting(Gs).

unInteresting([]).
unInteresting([G|Gs]) :- interesting(I), not member(G, I), unInteresting(Gs).

%5 Must be funny
funnyP(Gs) :- not unFunny(Gs).

unFunny([]).
unFunny([G|Gs]) :- funny(F), not member(G, F), unFunny(Gs).

%9 not mixing social party
noMixing(Gs) :- not mixing1(0,0,Gs).

mixing1(Dn, Rn, []) :- eval(Dn,Dr), eval(Rn,Rr), not Dr == 0, not Rr == 0.
mixing1(Dn, Rn, [G|Gs]) :- republican(G), mixing1(Dn, Rn+1, Gs).
mixing1(Dn, Rn, [G|Gs]) :- democrat(G), mixing1(Dn+1, Rn, Gs).
mixing1(Dn, Rn, [G|Gs]) :- independent(G), mixing1(Dn, Rn, Gs).


%7 Have connections
connections(Gs) :- connections1(Gs,Gs).

connections1(Party, [G|[]]) :- know(G,Y), member(Y,Party).
connections1(Party, [G|Gs]) :- know(G,Y), member(Y,Party), connections1(Party, Gs).

%8 cant dislike each other
noDislikes(Gs) :- not dislikes(Gs,Gs).

dislikes(Party, [G|[]]) :- dislike(G,Y), member(Y,Party).
dislikes(Party, [G|Gs]) :- dislike(G,Y), member(Y,Party).
dislikes(Party, [G|Gs]) :- dislikes(Party, Gs).



%No Duplicates
noDuplicates([]).
noDuplicates([G|Gs]) :- not member(G,Gs), noDuplicates(Gs).

duplicates([]).
duplicates([G|Gs]) :- duplicates(Gs), member(G,Gs).

%FInal thing to call - form: guests(People in party, number in party)
guests(Gs, N) :- length(Gs, N), gender(Gs), availability(Gs), interestingP(Gs), 
funnyP(Gs), equalGender(Gs), connections(Gs), noDislikes(Gs), noMixing(Gs), noDuplicates(Gs).
