(define (com lis1 lis2)
    (cond 
        ((null? lis1) '())
        ((member (car lis1) lis2) 
            (cons (car lis1) (com (cdr lis1) lis2))
        )
        (else (com (cdr lis1) lis2))
    )
)