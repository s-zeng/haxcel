# haxcel

A toy "spreadsheet" implementation. More precisely, it's an implementation of a 
strongly-typed interpreted language that supports a very small list of 
primitives:

- Boolean, int, floating, and string primitives
- Variables
- Arithmetic (including mixed integer/floating math)
- Sum function
- If-expressions
- Equality checking (as a function; I did not implement non-arithmetic infix 
  operators)

This is a toy implementation, so a number of useful features are not currently 
implemented (perhaps further work will come), such as:

- Non-arithmetic infix operators (e.g. comparisons, boolean operators, string 
  concatenation)
- Rows/columns/ranges
- Lambdas
- Cell formatting

## Running

The CLI supports three instructions:

- `e <cellname> <contents>`: Write `<contents>` into the cell `<cellname>`
- `p <cellname>`: Prints the raw contents of the cell
- `v <cellname>`: Prints the calculated value of the cell

I didn't implement readline semantics, so it is recommended to run this with 
`rlwrap`. To run, simply execute `stack run` or `rlwrap stack run` in the root 
of this repository. Tested using stack 2.7.5.

## Example

```
# rlwrap stack run
> e a1 3+4*5*6-7+8
> p a1
"3+4*5*6-7+8"
> v a1
Int 124
> e a2 0.4-100*3.8+7
> p a2
"0.4-100*3.8+7"
> v a2
Floating (-372.6)
> e a3 equal(a1, a2)
> v a3
False
> e a4 if(a3, false, true)
> v a4
True
> e a5 if(a4, a1-a2, sum(a1, a2, a1*a2))
> v a5
Floating 496.6
> e a1 -372.6
> v a5
Floating 138085.56
```

See `spec/Test.hs` and try running `stack test` to see the unit test that 
corresponds to this session
