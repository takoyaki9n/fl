nat_list([]).
nat_list([A|X]) :- nat(A), nat_list(X).
nat(z).
nat(s(X)) :- nat(X).
