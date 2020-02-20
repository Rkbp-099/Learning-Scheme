(define (fact n)
    (IF (<= n 1)
        1
        (* n (fact (- n 1)))
    )
)