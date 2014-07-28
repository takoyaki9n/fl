ins(X, Y, [X|Y]).
ins(X, [A|Y], [A|Z]) :- ins(X, Y, Z).

perm([], []).
perm([A|X], Z) :- perm(X, Y), ins(A, Y, Z).

sorted([]).
sorted([_]).
sorted([A, B| X]) :- A =< B, sorted([B| X]).

mysort(X, Y) :- perm(X, Y), sorted(Y), !.

