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
guestTest1(N, Gs) :- length(Gs,N), men(Gs).

%2
maleOrFemale([]).
maleOrFemale([G|Gs]) :- men(M), member(G,M), maleOrFemale(Gs).
maleOrFemale([G|Gs]) :- women(W), member(G,W), maleOrFemale(Gs).

%3
availability(Gs) :- availability1(friday, Gs).
availability(Gs) :- availability1(saturday, Gs).

availability1(_,[]).
availability1(Y, [G|Gs]) :- available(G, Y), availability1(Y, Gs).

%4
interestingPerson(Gs) :- not noInteresting(Gs).

noInteresting([]).
noInteresting([G|Gs]) :- interesting(I), not member(G, I), noInteresting(Gs).

%5
funnyPerson(Gs) :- not noFunny(Gs).

noFunny([]).
noFunny([G|Gs]) :- funny(F), not member(G, F), noFunny(Gs).

%6
%parity(Gs) :- length(Ws, Wn),length(Ms,Mn),Wn=Mn.

parity(Gs) :- parity1(0,Gs).

parity1(N,[]) :- eval(N,Nr), Nr == 0.
parity1(N,[G|Gs]) :- men(M), member(G,M), parity1(N+1,Gs).
parity1(N,[G|Gs]) :- women(W), member(G,W), parity1(N-1,Gs).

%7
connections(Gs) :- connections1(Gs,Gs).

connections1(Party, [G|[]]) :- know(G,Y), member(Y,Party).
connections1(Party, [G|Gs]) :- know(G,Y), member(Y,Party), connections1(Party, Gs).

%8
noDislikes(Gs) :- not dislikes(Gs,Gs).

dislikes(Party, [G|[]]) :- dislike(G,Y), member(Y,Party).
dislikes(Party, [G|Gs]) :- dislike(G,Y), member(Y,Party).
dislikes(Party, [G|Gs]) :- dislikes(Party, Gs).

%9
noMixing(Gs) :- not mixing1(0,0,Gs).

mixing1(Dn, Rn, []) :- eval(Dn,Dr), eval(Rn,Rr), not Dr == 0, not Rr == 0.
mixing1(Dn, Rn, [G|Gs]) :- republican(G), mixing1(Dn, Rn+1, Gs).
mixing1(Dn, Rn, [G|Gs]) :- democrat(G), mixing1(Dn+1, Rn, Gs).
mixing1(Dn, Rn, [G|Gs]) :- independent(G), mixing1(Dn, Rn, Gs).

%No Duplicates
noDuplicates([]).
noDuplicates([G|Gs]) :- not member(G,Gs), noDuplicates(Gs).

duplicates([]).
duplicates([G|Gs]) :- duplicates(Gs), member(G,Gs).

%FINAL - form: guests(People in party, number in party)
guests(Gs, N) :- length(Gs, N), maleOrFemale(Gs), availability(Gs), interestingPerson(Gs), 
funnyPerson(Gs), parity(Gs), connections(Gs), noDislikes(Gs), noMixing(Gs), noDuplicates(Gs).
