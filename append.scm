(define (append lis1 lis2)
    (cond 
        ((null? lis1) lis2)
        (else (cons (car lis1) (append (cdr lis1) lis2)))
    )
)