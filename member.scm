(define (member atm lis1)
    (cond 
        ((null? lis1) "false")
        ((eq? atm (car lis1)) "true")
        (else (member atm (cdr lis1)))
    )
)