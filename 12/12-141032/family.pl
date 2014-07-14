male(kobo).
male(koji).
male(iwao).
female(sanae).
female(mine).
parent(kobo,koji).
parent(kobo,sanae).
parent(sanae,iwao).
parent(sanae,mine).

ancestor(X, Y) :- ancestor(Z, Y), parent(X, Z).
ancestor(X, Y) :- parent(X, Y).

