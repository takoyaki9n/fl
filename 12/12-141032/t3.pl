win(P, B) :- 
	opposite(B, A), 
	win_game(P, B), 
	\+ win_game(P, A), !.
%%	nl, write('win  '), write(B), write(P), nl, !.
win(P, B) :- 
	movable(P), !,
	opposite(B, A), 
	move(P, B, Q), 
	lose(Q, A), !.
%%	write('win  '), write(B), write(P), nl.

lose(P, B) :- 
	opposite(B, A), 
	win_game(P, A), 
	\+ win_game(P, B), !.
%%	nl, write('lose '), write(B), write(P), nl, !.
lose(P, B) :- 
	movable(P), !,
	opposite(B, A), 
	\+ (move(P, B, Q), 
	(lose(Q, A); tie(Q, A))), !.
%%	write('lose '), write(B), write(P), nl.

tie(P, B) :- 
	\+ win(P, B), \+ lose(P, B), !.
%%	write('tie  '), write(B), write(P), nl.

movable([[z|_]|_]).
movable([[_|X]|P]) :- movable([X|P]).
movable([[_]|P]) :- movable(P).

move([[z|X]|P], B, [[B|X]|P]).
move([[A|X]|P], B, [[A|Y]|Q]) :- move([X|P], B, [Y|Q]).
move([[A]|P], B, [[A]|Q]) :- move(P, B, Q).

opposite(x, c).
opposite(c, x).

win_game(X, A) :- 
	(col(X, Z); row(X, Z); crs(X, Z)),
	win_line(Z, A),
	A \= z, !.

win_line([A], A).
win_line([A|X], A) :- win_line(X, C), A = C, !.
win_line([_|_], z).

col(X, Y) :- nth1(_, X, Y).
row(X, Y) :- maplist(nth1(_), X, Y).
crs(X, Y) :-
	length(X, N),
	(range(1, N, Z); range_rev(1, N, Z)),
	maplist(nth1, Z, X, Y).

range(I, N, []) :- I > N.
range(I, N, [I|X]) :- I =< N, J is I + 1, range(J, N, X), !.
range_rev(I, N, []) :- I > N.
range_rev(I, N, [N|X]) :- I =< N, M is N - 1, range_rev(I, M, X), !.

/*
cxc
-xx
xcc
*/