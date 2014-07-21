# Introduction to SCM-Prolog

1. Legend
2. Prolog Terms
	2.1. Prolog Variable
	2.2. Prolog Atom
	2.3. Prolog Number
	2.4. Prolog String
	2.5. Prolog Arguments
	2.6. Prolog List
	2.7. Prolog Fact
	2.8. Prolog Rule
3. Variables Pool
	3.1. Main Pool
	3.2. Word Pool
4. Unification
5. Evaluation
6. Runtime optimizations

1. Legend
=========

Each Prolog Term and process in the documentation is marked in a specific way.
The Terms:
	Varialbe: single words, starting with capital letter: X, Some.
	          A Variable could be "random" - that means that it has a random name and its value is not important to the program. They are written as _.
	Atom: single words, starting with small letter: x, some;
	      or a string, surrounded by ' ': 'this is an atom too', 'Yeah - atoms'	
	Number: numbers are normal numbers: 42, 3.14.
	String: as one would guess: "string", "another stirng".
	Arguments: Terms, surrounded by ( ) and separated by ',': (X, atom).
	List: Lists have two parts: head and tail.
	      The head is written as Prolog Terms, surrounded by [ ] and separated ny ',': [X, atom].
	      If the tail is not empty it is written right after the elements of the head, separated with '|': [X, atom | [another_list]]
	Facts: they have two parts: Atom and Arguments. They are written together: member(A, X, [A|X]).
	Rules: they have two parts: Fact and body. The body is a Prolog Term. The two parts are written together, separated by ':-':
	       member(A, [_ | X]) :- member(A, X).
