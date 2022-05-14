# exp - a programming language

## Core features

* A program is just an expression. All the syntax which is available
  locally (in a let binding or function body) can be used at the global
  level, and vice versa.
* All data is immutable and there are no cyclic references.
* A functional language, but not a purely functional one - functions can
  have side effects. Evaluation is strict.
* The language is dynamically typed, but it has a stronger type system
  than a typical dynamic language - there are no implicit conversions,
  and comparing or pattern matching values of different types is a
  runtime type error.

## Syntax

The syntax is based on Lisp S-expressions, but with one innovation: the
semicolon can be used to group values at the end of an S-expression. For
example, this S-expression:

    (a (b (c (d))))

can equivalently be written as:

    (a; b; c; d)

This eliminates the large number of closing parentheses typical of Lisp.

## Examples

### Let binding

    let $x 5 (add x x)
    => 10

### Function definition

      let $f (
        fn $x $y (add (add x x) y)
      )
    ; f 5 7
    => 17

### Names with whitespace

    let $[my variable] 5 [my variable]
    => 5

### Strings

    let $s1 "[hello, ]; let $s2 "world; concat s1 s2
    => "[hello, world]

### If expressions

    if true 1 2
    => 1

### Vectors

      let $v #(1 2)
    ; length v
    => 2

    let $v #(1 2)
    ; get v 1
    => 2

### Pattern matching

      let $v #("b 2)
    ; match v
        #("a $x) x
        #("b $x) (add x x)
    => 4

### Closures

      let $iterate (
        fn $f $count $x
      ; if (eq 0 count) x
      ; f (iterate f (sub count 1) x)
      )
    ; let $y 1
    ; iterate (fn $x; add y x) 5 2
    => 7

### Algebraic data types

      data ($cons _ _) $nil
    ; let $list (
        fn $vec
      ; let $build (
          fn $result $i
        ; if (eq i 0) result
        ; build (cons (get vec; sub i 1) result) (sub i 1)
        )
      ; build nil (length vec)
      )
    ; let $sum (
        fn $list
      ; match list
          (cons $x $xs) (add x; sum xs)
          nil 0
      )
    ; sum (list #(1 2 3 4 5))
    => 15
