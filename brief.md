# Some notes on the Scheme programming language

## Intro
* MIT, 1975
* __r5rs__, r6rs, r7rs
* LISP dialect or independent language?

## Differences from LISP

* `#t, #f` VS. `T, NIL`
* `NIL /= #f`
* Predicates: `null? pair? string=?`
* Coercion: `string->list symbol->string`
* Each symbol has only __one__ value
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
* Examples:
```scheme
    ; FACTORIAL
    (define (factorial n)
        (if (= n 0)
            1
            (* n (factorial (- n 1)))
        )
    )

    (define (factorial-cps n cc)
        (if (= n 0)
            (cc 1)
            (factorial-cps (- n 1) (lambda (x) (cc (* x n))))
        )
    )

    ; APPEND
    (define (append1 l1 l2)
        (if (null? l1)
            l2
            (cons (car l1) (append1 (cdr l1) l2))
        )
    )

    (define (append-cps l1 l2 cc)
        (if (null? l1)
            (cc l2)
            (append-cps (cdr l1) l2 (lambda (x) (cc (cons (car l1) x))))
        )
    )
```
* CPS profit: tail recursion => optimization
* Hard to read and write, compilers can do it automatically (example: ML)

## CALL/CC

* Continuations are first-class in Scheme, just like functions (can be saved, revoked, etc.)
* There's `call/cc` to work with them
* Example:
```scheme
    (define (product-cc l)
        (define (receiver cc)
            (letrec
                ((product (lambda (l)
                    (cond
                        ((null? l) (print "NIL") 1)
                        ((= 0 (car l)) (cc 0))
                        (else (print (car l)) (* (car l) (product (cdr l))))
                    )
                )))
                (product l)
            )
        )
        (call/cc receiver)
    )
```
* Other use cases:
    * escape from infinite loop
    * exceptions
    * reenter "deep" recursion (good for debugging)
    * co-routines
    * stack of continuations => backtracking

## Syntax-rules
  * There's `defmacro` and QQ
  * Syntax-rules are more powerful
  * Example (explains pattern language):
```scheme
  (define-syntax my-and
      (syntax-rules ()
          ((my-and) #t)
          ((_ x) x)
          ((_ e1 e2 ...) (if e1 (my-and e2 ...) #f))
      )
  )
```
  * Can be used to define functions (special forms) that shouldn't evaluate its arguments

## Links

* [r5rs](http://www.schemers.org/Documents/Standards/R5RS/)
* [CPS (Wikipedia)](https://en.wikipedia.org/wiki/Continuation-passing_style)
* [Call/cc patterns (PDF, en)](http://repository.readscheme.org/ftp/papers/PLoP2001_dferguson0_1.pdf)
* [Call/cc patterns (ru)](http://fprog.ru/lib/ferguson-dwight-call-cc-patterns/)
* [Syntax-rules examples and exercises](http://www.shido.info/lisp/scheme_syntax_e.html)
* [Macros in Scheme](https://en.wikibooks.org/wiki/Scheme_Programming/Macros)
