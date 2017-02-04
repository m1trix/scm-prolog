#Introduction to SCM-Prolog

The documentation is separated into several parts:

  - Prolog Terms:
    - Prolog Variable,
    - Prolog Atom,
    - Prolog Number,
    - Prolog String,
    - Prolog Arguments,
    - Prolog List,
    - Prolog Fact,
    - Prolog Rule,
    - Prolog Conjunction,
    - Prolog Disjunction,
    - Prolog Expression,
    - Prolog Function.
  - Working process:
    - Parsing
    - Interpreting:
      - Variable Pools:
	- Main Pool,
	- Work Pool,
      - Unification,
      - Evaluation,
      - Backtracking,
      - Debugging,
      - Tracing.
  - Optimizaitons:
    - optimize_pool,
    - optimize_math,
    - optimize_compile

##Prolog Terms
A Term is called every object that Prolog uses in order to work.

####Prolog Variable
A Variable in Prolog is a Term, that has no value upon its creation. During the Work process, variables are evaluated. Once a Variable gets a value, it cannot be changed. From that point on, that Variable is threated as its value. Each Variable in prolog has a name: a single word, starting with a capital letter: *X*, *Target*. There can be a random variable: it's name is just *'_' (underline symbol)*. Those Variables recieve randomly generated names and their values are of no importance to the Working process.

####Prolog Atom
An Atom in Prolog is an object that has a name: either it is a single word, starting with a small letter: *x*, *target*, or it's a string, surrounded by single quotes: *'this is an atom'*,*'This too!'*.

####Prolog Number
Prolog Numbers are like any numbers: *42*, *3.14*.

####Prolog String
Prolog Strings are like any strings: *"this is a string"*

####Prolog Arguments
Prolog Arguments list is an ordered list of Terms separated by commas and surrounded by brackets: *(Var, atom, "string")*.

####Prolog List
Prolog Lists are lists of Prolog Terms. They have two parts: **head** and **tail** - both of them could be empty. The **head** is a ordered list of Terms, separated by commas and surrounded by square braces: *[Var, atom, "string"]*. The **tail** could by a Prolog Variable or a Prolog List. If it's an empty list, it's not written. Otherwise, it's written after the elements of the head, separated from them with '|' symbol: *[Var, atom, "string" | Tail]*, *[Var, atom, "string" | [another_list]]*.

####Prolog Fact
A Prolog Fact is a Term composed of an Atom and an Arguments list. They are written alltogether: *member(A, X, [A | X])*.

####Prolog Rule
A Prolog Rule is a Term composed of a Fact and a Term, separated by ':-'. The fact is called **head**, the Term is called **body** and the ':-' - **neck**. The meaning of it is that: *"If the body is true, then the head is true."*

##Working Process
Prolog recieves quries from the user and tries to unify them with Rules from the knowledge base.

####Interpreting
After the query is "translated" from a string to a Prolog Term, the Interpreter tries to resolve it. That is done by matching goals from the query to rules, facts and methods from the Knowledge Base. 

#####Variables Pool
During the work tow Variables Pools are created: Main Pool and Work Pool. The Main pool is a set of all Variables names, included in the initial query. Those are the important Variables. The Work Pool is a map where to the name of each Variable created in the Work Process is mapped it's value (if not evaluated, the Variable is not included in the map). The Work pool is constantly changing, while the Main Pool remains exactly the same.

######Unification
