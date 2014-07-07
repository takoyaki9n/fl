%% member(H, T)
member(H, [H| _]).
member(H, [_| T]) :- member(H, T).

%% remove(A, X, Y)
remove(_, [], []).
remove(A, [A|X], Y) :- remove(A, X, Y).
remove(A, [B|X], [B|Y]) :- remove(A, X, Y), \+ A = B.

has_edge(X, Y, [{X, N}| _]) :- member(Y, N).
has_edge(X, Y, [_| E]) :- has_edge(X, Y, E).

%% simple_path(P, V, E)
simple_path([X], V, _) :- member(X, V).
simple_path([X, Y| P], V, E) :- 
	member(X, V), 
	has_edge(Y, X, E),
	remove(X, V, U),
	remove({X, _}, E, F),
	simple_path([Y| P], U, F), 
	\+ member(X, [Y| P]).

contains(_, []).
contains(X, [A|Y]) :- member(A, X), contains(X, Y).

hamilton(V, E) :- simple_path(P, V, E), contains(P, V).