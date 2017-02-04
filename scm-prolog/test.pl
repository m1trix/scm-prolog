% member(Element, List) :- Element is member of List.
member(Element, [Element | _]).
member(Element, [_ | RestList]) :- member(Element, RestList).

% insert(Element, List, NewList) :- NewList is the result of inserting Element in List.
insert(Element, List, [Element | List]).
insert(Element, [Head | List], [Head | NewList]) :- insert(Element, List, NewList).


% nat(N) :- N is a natural nubmer.
nat(0).
nat(A) :- nat(B), A is B + 1.

% remove(Element, List, NewList) :- NewList is the result of removing Element from List.
remove(Element, [Element | List], List).
remove(Element, [Head | List], [Head | NewList]) :- remove(Element, List, NewList).

perm([], []).
perm(List, [Head | Perm]) :- remove(Head, List, Rest), perm(Rest, Perm).
