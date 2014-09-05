% member(Element, List) :- Element is member of List.
member(Element, [Element | _]).
member(Element, [_ | RestList]) :- member(Element, RestList).

% insert(Element, List, NewList) :- NewList is the result of inserting Element in List.
insert(Element, List, [Element | List]).
insert(Element, [Head | List], [Head | NewList]) :- insert(Element, List, NewList).


% nat(N) :- N is a natural nubmer.
nat(0).
nat(A) :- nat(B), A is B + 1.
