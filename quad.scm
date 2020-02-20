(define (quad a b c)
    (let (
            (d ( / (sqrt (- (* b b) (* a c 4))) (* a 2)))
            (sam (/ (- 0 b) (* a 2)))
        )
        (list (- sam d) (+ sam d))
    )
)