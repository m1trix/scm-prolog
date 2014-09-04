% member(Element, List) :- Element is member of List.
member(Element, [Element | _]).
member(Element, [_ | RestList]) :- member(Element, RestList).