# exp programming language

## Core features

* Strongly, dynamically typed
* All data is immutable
* Expression based, "everything is an expression"
* First class functions
* Algebraic data types and pattern matching

## Example

    [! Defines a linked list as an algebraic data type ]
    data ($cons _ _) $nil

    [! Function to convert an array to a list ]
    let $list
      fn $array
      loop (var $i 0) (var $result nil)
      if (leq (length array) i) result
      next (add i 1)
      cons (get array; sub (length array); add i 1) result

    [! Function to sum a list of numbers using pattern matching and
       recursion
    ]
    let $sum
      fn $list
      match list (case (cons $x $xs); add x; sum xs) (case nil 0)

    sum (list; @ 1 2 3 4 5)

For other examples, see examples/ directory.
