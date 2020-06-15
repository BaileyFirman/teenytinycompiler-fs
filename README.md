# teenytinycompiler-fs

This is an F# implementation of Austin Z. Henley's Teeny Tiny compiler.

Read the original tutorial: [Let's make a Teeny Tiny compiler, part 1](http://web.eecs.utk.edu/~azh/blog/teenytinycompiler1.html)

Currently the code is only complete up to part one of Austin's tutorial with more to come soon.

Like the original python implementation It will support:
  - Numerical variables
  - Basic arithmetic
  - If statements
  - While loops
  - Printing text and numbers
  - Input numbers
  - Labels and goto
  - Comments

Example code:
```
PRINT "How many fibonacci numbers do you want?"
INPUT nums

LET a = 0
LET b = 1
WHILE nums > 0 REPEAT
    PRINT a
    LET c = a + b
    LET a = b
    LET b = c
ENDWHILE	
```