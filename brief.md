# Introduction to the Scheme programming language

## Intro
* MIT, 1975
* __r5rs__, r6rs, r7rs
* LISP dialect or independent language?

## Differences from LISP

* `#t, #f` VS. `T, NIL`
* `NIL /= #f`
* Predicates: `null? pair? string=?`
* Coercion: `string->list symbol->string`
* Each symol has only __one__ value
* No dynamic binding
* `defun` VS. `define`
* Real first-class functions:
    ```lisp
    (funcall #'(lambda (x) (+ x 1)) 1)
    ```
    VS.
    ```scheme
    ((lambda (x) (+ x 1)) 1)
    ```

## CPS

* Continuation is like context that can be represented as function (argument = results of current evaluation)
* Function can be passed to other function as argument
* Example: `append` vs `append-cps`:
```scheme
TODO
```
* CPS profit: tail recursion => optimization
* Hard to read and write, compilers can do it automatically (example: ML)

## CALL/CC

* Continuations are first-class in Scheme, just like functions (can be saved, revoked, etc.)
* There's `call/cc` to work with them
* Example: `product`:
```scheme
TODO
```
* Other use cases:
    * excape from infinite loop
    * exceptions
    * reenter "deep" recursion (good for debugging)
    * coroutines
    * stack of continuations => backtracking

## Syntax-rules

## Links

* [r5rs](http://www.schemers.org/Documents/Standards/R5RS/)
* [CPS (Wikipedia)](https://en.wikipedia.org/wiki/Continuation-passing_style)
* [Call/cc patterns (PDF, en)](http://repository.readscheme.org/ftp/papers/PLoP2001_dferguson0_1.pdf)
* [Call/cc patterns (ru)](http://fprog.ru/lib/ferguson-dwight-call-cc-patterns/)
