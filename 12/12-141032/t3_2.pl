win(P, B) :- 
	opposite(B, A),
	win_game(P, B), !,
	\+ win_game(P, A).
%%	nl, write('win  '), write(B), write(P), nl, !.
win(P, B) :- 
	move(P, B, _), !,
	opposite(B, A), 
	move(P, B, Q), 
	lose(Q, A), !.
%%	write('win  '), write(B), write(P), nl.

lose(P, B) :- 
	opposite(B, A), 
	win_game(P, A),
	\+ win_game(P, B).
%%	nl, write('lose '), write(B), write(P), nl, !.
lose(P, B) :- 
	move(P, B, _), !,
	opposite(B, A), 
	\+ (move(P, B, Q), 
	(lose(Q, A); tie(Q, A))), !.
%%	write('lose '), write(B), write(P), nl.

tie(P, B) :- 
	\+ win(P, B), \+ lose(P, B), !.
%%	write('tie  '), write(B), write(P), nl.

move([[z|X]|P], B, [[B|X]|P]).
move([[A|X]|P], B, [[A|Y]|Q]) :- move([X|P], B, [Y|Q]).
move([[A]|P], B, [[A]|Q]) :- move(P, B, Q).

opposite(x, c).
opposite(c, x).

win_game([[A,A,A],[_,_,_],[_,_,_]], A).
win_game([[_,_,_],[A,A,A],[_,_,_]], A).
win_game([[_,_,_],[_,_,_],[A,A,A]], A).
win_game([[A,_,_],[A,_,_],[A,_,_]], A).
win_game([[_,A,_],[_,A,_],[_,A,_]], A).
win_game([[_,_,A],[_,_,A],[_,_,A]], A).
win_game([[A,_,_],[_,A,_],[_,_,A]], A).
win_game([[_,_,A],[_,A,_],[A,_,_]], A).
