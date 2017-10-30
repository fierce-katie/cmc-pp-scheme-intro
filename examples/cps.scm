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

(define (reverse1 l)
    (if (null? l)
        l
        (append (reverse1 (cdr l)) (list (car l)))
    )
)

(define (reverse-cps l cc)
    (if (null? l)
        (cc l)
        (reverse-cps (cdr l)
                     (lambda (x) (append-cps x (list (car l)) cc))
        )
    )
)
