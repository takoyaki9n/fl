male(namihei).
male(masuo).
male(katsuo).
male(tarao).
female(fune).
female(sazae).
female(wakame).
parent(sazae, namihei).
parent(katsuo, namihei).
parent(wakame, namihei).
parent(sazae, fune).
parent(katsuo, fune).
parent(wakame, fune).
parent(tarao, masuo).
parent(tarao, sazae).

sibling(X, Y) :- parent(X, Z), parent(Y, Z), \+ X = Y.

bloodrelative(X, Y) :- ancestor(X, Y).
bloodrelative(X, Y) :- ancestor(Y, X).
bloodrelative(X, Y) :- ancestor(X, Z), ancestor(Y, Z), \+ X = Y.

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

