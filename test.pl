% insert(Element, List, Result) :- Result is the result of inserting Element in List.
insert(Element, [], [Element]).
insert(Element, List, [Element | List]).
insert(Element, [Head | List], [Head | Result]) :- insert(Element, List, Result). 