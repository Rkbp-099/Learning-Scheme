  (define merge-lists
    (lambda (l1 l2)
      (if (null? l1)
          l2
          (if (null? l2)
              l1
              (if (< (car l1) (car l2))
                  (cons (car l1) (merge-lists (cdr l1) l2))
                  (cons (car l2) (merge-lists (cdr l2) l1)))))))

;   (define even-numbers
;     (lambda (l)
;       (if (null? l)
;           '()
;           (if (null? (cdr l))
;               '()
;               (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))
  (define odd-numbers
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              (list (car l))
              (cons (car l) (odd-numbers (cdr (cdr l))))))))


  (define merge-sort
    (lambda (l)
      (if (null? l)
          l
          (if (null? (cdr l))
              l
              (merge-lists
                (merge-sort (odd-numbers l))
                (merge-sort (odd-numbers (cdr l) )))))))


                (define (binary-search lst elem)
                (define mid (floor (/ (length lst) 2)))
                (define midelem (list-ref lst mid))
                
                (if (null? lst)
                   #f
                   (cond
                     ((and (= (length lst) 1) (not (equal? (first lst) elem))) #f)
                     ((= midelem elem) #t)
                     ((> midelem elem) (binary-search (take lst mid) elem))
                     ((< midelem elem) (binary-search (drop lst mid) elem))
                     (else #f)
                   )
                 )
              )

(define msort 
    (lambda (list)
    (define mid (floor (/ (length list) 2))) 
        (cond
            ((null? list) '())
            ((null? (cdr list)) list)
            (else (merge (msort (take list mid)) (msort (drop list mid))))
            )))

(define merge (lambda (list1 list2)
            (cond
                ((null? list1) list2)
                ((null? list2) list1)
                ((> (car list1) (car list2)) (cons (car list2) (merge list1 (cdr list2))))
                (else (cons (car list1) (merge list2 (cdr list1))))
            )
))

(define less (lambda (list piv)
                (cond
                    ((null? list) list)
                    ; ((= (length list) 1))
                    ((< (car list) piv) (cons (car list) (less (cdr list) piv)))
                    (else (less (cdr list) piv))
                    )
))

(define greater (lambda (list piv)
                (cond
                    ((null? list) list)
                    ; ((= (length list) 1))
                    ((> (car list) piv) (cons (car list) (greater (cdr list) piv)))
                    (else (greater (cdr list) piv))
                    )
))

(define (getRep elem list)
    (cond
        ((null? list) list)
        ((= (car list) elem) (cons elem (getRep elem (cdr list))))
        (else
            (getRep elem (cdr list)) 
            )
        
        )
)

(define qsort
    (lambda (list)
        (cond
            ((or (null? list) (null? (cdr list))) list)
            (else 
                (append (qsort (less list (car list))) (append (getRep (car list) list) (qsort (greater list (car list))))))
            )
        )
    )

(define (selec list)
    (cond 
        ((or (null? list) (null? (cdr list))) list)
        (else 
            (append (getRep (apply min list) list ) (selec (greater list (apply min list))))
            )
        )
    
    )