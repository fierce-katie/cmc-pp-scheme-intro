(define-syntax my-or-wrong
    (syntax-rules ()
        ((my-or) #f)
        ((_ x) x)
        ((my-or e1 e2 ...) (if e1 e1 (my-or-wrong e2 ...)))
    )
)

(define-syntax my-or
    (syntax-rules ()
        ((my-or) #f)
        ((_ x) x)
        ((my-or e1 e2 ...)
            (let ((tmp e1)) ; should be evaluated only once
                (if tmp tmp (my-or e2 ...))
            )
        )
    )
)

(define-syntax my-and
    (syntax-rules ()
        ((my-and) #t)
        ((_ x) x)
        ((_ e1 e2 ...) (if e1 (my-and e2 ...) #f))
    )
)

(define-syntax my-cond
    (syntax-rules (else)
        ((_) (print "No 'else' clause in 'my-cond'"))
        ((_ (else . body)) (begin . body))
        ((_ (bool-expression . body) . rest)
            (if bool-expression
                (begin . body)
                (my-cond . rest)
            )
        )
    )
)


