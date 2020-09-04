
# teenytinycompiler-fs

  

This my F# implementation of [Austin Z. Henley's](http://web.eecs.utk.edu/~azh/index.html) [Teeny Tiny compiler](https://github.com/AZHenley/teenytinycompiler).

  

Read the awesome original tutorials by Austin: [Let's make a Teeny Tiny compiler, part 1](http://web.eecs.utk.edu/~azh/blog/teenytinycompiler1.html) as well as [part 2](http://web.eecs.utk.edu/~azh/blog/teenytinycompiler2.html) and [part 3](http://web.eecs.utk.edu/~azh/blog/teenytinycompiler3.html)

  

Like the original python implementation the code is split into folders (part1, part2, part3) that correspond with the complete code from the parts of the tutorial except in F#. See part3 for the finished compiler.

  

Like the original python implementation it supports:

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