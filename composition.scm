(define (mul a)
    (* a 10)
)

(define (square x)
    (* x x)
)

(define (comp f g)
    ((lambda (x) (f (g x))))
)

()