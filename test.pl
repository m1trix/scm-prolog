% member(Element, List) :- Element is member of List.
member(Element, [Element | _]).
member(Element, [_ | RestList]) :- member(Element, RestList).

% insert(Element, List, NewList) :- NewList is the result of inserting Element in List.
insert(Element, List, [Element | List]).
insert(Element, [Head | List], [Head | NewList]) :- insert(Element, List, NewList).

% perm(List, NewList) :- NewList is a permutation of the elements of List.
perm([SingleElement], [SingleElement]).
perm([FirstElement | RestList], NewList) :- perm(RestList, PermutatedList), insert(FirstElement, PermutatedList, NewList).