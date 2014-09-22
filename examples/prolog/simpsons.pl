
child(bart,homer).

child(homer,abe).

child(maggie,homer).

grandchild(X,Y) :-
    child(X,Z),
    child(Z,Y).
