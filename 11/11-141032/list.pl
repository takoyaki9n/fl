reverse_aux([], Y, Y).
reverse_aux([A|X], Y, Z) :- reverse_aux(X, [A|Y], Z).
reverse(X, Y) :- reverse_aux(X, [], Y).

append([], Y, Y).
append([A|X], Y, [A|Z]) :- append(X, Y, Z).

concat([], []).
concat([A|X], Y) :- concat(X, Z), append(A, Z, Y).
