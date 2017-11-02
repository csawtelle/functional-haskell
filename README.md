This is a project for myself to learn functional Haskell

### Why am I learning Haskell and Functional Programming?

Reliability - catch errors at compile instead of runtime

Tractability - easily breaks down the solution into chunks

Composability - straight forward to combine individual pieces

Maintainability - easy to maintain and find bugs

Concurrency - Support for parallel programming

Abstraction - Easily model software from low to high

Honestly, would also like to improve my ability to write/use python lambdas
and list comprehensions by mastering functional programming in Haskell.


### What makes programming 'functional'?

Computation - evaluate a function based only on the input and output

A typical flow of imperitive programming:
1) Take in a value
2) Save it to a variable
3) Modify it algorithmically
4) Possibly a loop of some condition
5) Return the value 

Problems: Mutability, evaluation order is important

A typical functional program:
1) Uses expressions and declarations
2) Avoids changing global state
3) Avoids modifying variables (mutable)
4) avoids loops
5) Emphasizes recursion and high order abstractions

referential transparency - replace def with reference

Functions are first class values in functional programming
This means that functions can take functions as arguments, 
return functions, and functions can be joined together


Traversal - mapping variables
Reduction - iterate over collection with summary
filtering - iterate and extract subset
composition - join functions to create larger functions


### Whats special about Haskell?

1) All functions are pure: The return is only determined by input with no side effects ( This is Haskell Specific) 
2) interleaving inlining
3) non-strict evaluation (optional)
4) call by need (memoized) lazy evaluation
5) statically typed
  a) catch defects
  b) effcient code
  c) types are deleted at compile

### Pragmatic Haskell
1) Whitespace sensetive (pythonish)
2) Curly braces are sometimes supported
3) pure datastructures / non-strict eval requires garbage collector
4) GHC has a great GC
5) externally allocated memory is well supported
6) naming conventions
  a) medial capital 
  b) Initial Capital: Type, Type Constrictors, Data Constructors, Type Classes
  c) Initial lowercase: functions, values, variables, and type variables
  d) Type and Data Constrictors can share names (different namespaces)
  e) operators are spelled with symbols ( + - etc) can prefix with parenthisis 

### Whitespace layout and scoping
Haskell is whitespace sensetive like python
Whitespace matters in the following areas
Groups must be on the same indentation
99% of the time, a "natural" intendentation will be the correct format

Scoping
1) variables inside functions blocks are scoped inside and not accessible outside thier function


Where is similiar to "let" except names are introduced after instead of before
Ex.
let x = 5, do something with x
do something with x while x = 5

case of (need more explanation)

Braces:
  can group by braces instead of by whitespace
  let x = 5
      y = 1
or
  let { x = 5; y = 1; }

  Inside the braces, you can do whatever you want in terms of whitespace


### GHCi and Interactive haskell
The glorious glasgow haskell compilation system interactive

Prelude :h (help)
Prelude :! (executes anything following in the parent shell like Prelude :!ls)
Prelude x =5 (sets a variable in the interactive shell)
Prelude :type x (to get variable type)
Prelude :kind Num (shows kind of type, types have "kinds" not values)
Prelude :info Num (shows all the info about a kind)
Prelude :load Program (loads program in interpreter mode)
-- Can look at types of values, info of values


References:
https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf
https://www.udemy.com/learning-path-haskell-functional-programming-and-haskell/

