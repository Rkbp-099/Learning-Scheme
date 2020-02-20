
Leap Year
(define (leap? year)
    (cond
      ((zero? (modulo year 400)) #t)
      ((zero? (modulo year 100)) #f)
      (else (zero? (modulo year 4)))
    )
)

Atom is in list
(define (member atm list)
   (cond
       ((null? list) #f)
       ((eq? atm car(list)) #t)
       (else (member atm cdr(list)))
   )
)

Compare 2 Simple list are equal
(define (eqli list1 list2)
   (cond
     ( (null? list1) (null? list2) #t)
     ( (null? list2) #f)
     ( (eqv? (car list1) (car list2))
      (eqli cdr(list1) cdr(list2)) )
     ( else #f)
   )
)  //Not working for equal list

Comparing 2 general lists
(define (equal list1 list2)
   (cond
    ((not (list? list1)) (eq? list1 list2))
    ((not (list? list2)) #f)
    ((null? list1) (null? list2) #t)
    ((null? list2) #f)
    ((eq? car(list1) car(list2))
         (equal cdr(list1) cdr(list2))
      )
      (else #f)
   )
)

Appending 2 List
(define (append list1 list2)
   (cond
      ((null? list1) list2)
      (else( cons car(list1) (append cdr(list1) list2) )
    )
)

Quadratic Roots
(define (quadratic_roots a b c)
      (let
        (
        (root_part_over_2a (/ (sqrt (- (* b b ) (* 4 a c))) (* 2 a)))
            (minus_b_over_2a   (/ (- 0 b) (* 2 a)))
        )
        (
           list (+ minus_b_over_2a root_part_over_2a) (- minus_b_over_2a root_part_over_2a)
        )
      )
)

Normal Factorial
(define (fact n)
      (if(<= n 0)
         1
         (* n (fact (- n 1)))
      )
)

Tail Recursion Factorial
(define (facthelper n factpartial)
   (
     if(<= facthelper 0)
         factpartial
         (facthelper((- n 1) (* n factpartial)))
   )
)

(define (fact n)
    (facthelper n 1)
)

Gcd of 2 numbers
(define gcd
   (lambda(a b)
    (cond
       ((zero? b) a)
       (else (gcd b (modulo a b)))
     )
   )
)


Fun 91
(define fun
   ( lambda(n)
    (cond
        ((> n 100) (- n 10))
        (else (fun (fun (+ n 11))))
    )
   )
)

Lambda Expression
(define fun
   (lambda(x y)
      (+ (* y y) x)
   )
)

Length of list
(define fun
      (lambda(l)
        (cond
           ((null?l) 0)
           (else (+ 1 fun(cdr(l))))
        )
      )
)

Concatenating 2 List
(define concat
(lambda(l1 l2)
(cond
((null? l1) l2)
        (else (cons (car l1) (concat (cdr l1) l2)))
)
)
)

Reverse a list
(define rev
     (lambda(l cv)
       (cond
      ((null? l) cv)
      (else
      (rev (cdr l) (cons (car l) cv))
      )
       )
     )
)
(define help
   (lambda(l)
      (rev l '())
   )
)

Membership of atom
(define member
    (lambda(atm l)
     (cond
       ((null? l)  #f)
       ((eq? atm (car l)) #t)
       (else (member atm  (cdr l)))
     )
    )
)

Power function
(define expe
  (lambda(a n)
   (cond
    ((zero? n) 1)
    ((even? n) (expe (* a a) (/ n 2)))
    (else (* a (expe a (- n 1)))
   )
  )
 )
)

Composite function
(define compose
    (lambda(f g)
     (
       lambda(x)
       (f (g x))
     )
    )
)

Map
(map (lambda (x) (* x x x)) '(1 2 3))

Adding list of numbers
(define (adder l)
   (cond
      ((null? l) 0)
      (else (eval (cons '+ l)))
   )
)

Compare Two Lists
(define (check l1 l2)
   (equal? l1 l2)
)

Maximum of list
(define (max l)
   (cond
      ((null? l) '())
      ((null? (cdr l)) (car l))
      ((> (car l) ( max (cdr l))) (car l))
      (else (max (cdr l)))
   )
)

Return List with last element deleted
(define del
   (lambda(l)
     (cond
       ( (null? (cdr l)) '())
       ( else (cons (car l) (del (cdr l))) )
     )
   )
)

Q10-Sebesta
(define (func number1 number2 numberlist)
(COND
((NULL? numberlist) '())
((= number1 (CAR numberlist)) (CONS number2 (func number1 number2 (CDR numberlist))))
(ELSE(CONS (CAR numberlist) (func number1 number2 (CDR numberlist))))
)
)

Q12- List Structural Equality
(define (structure list1)
(if (null? list1)
list1
(if (list? (car list1))
(cons (structure (car list1)) (structure (cdr list1)))
(structure (cdr list1))
)
)
)
(define (func list1 list2)
(equal? (structure list1) (structure list2))
)

Sum 2 List
(define (su numberlist1 numberlist2)
    (if
    (NULL? numberlist1) '()
    (CONS (+ (CAR numberlist1) (CAR numberlist2)) (su (CDR numberlist1) (CDR numberlist2)))
    )
)

union of list
(define (member atm list)
(COND
((NULL? list)(NOT #t))
((EQ? atm (CAR list))#t)
(ELSE(member atm (CDR list)))
)
)
(define (union setlist1 setlist2)
(COND
 ((NULL? setlist1)setlist2)
 ((member (CAR setlist1) setlist2) (union (CDR setlist1) setlist2))
 (ELSE(CONS(CAR setlist1)setlist2) (union (CDR setlist1) setlist2))
)
)


quicksort
(define (partition compare l1)
      (cond
         ((null? l1) '())
         ((compare (car l1)) (cons (car l1) (partition compare (cdr l1))))
         (else (partition compare (cdr l1)))))

   (define (quicksort l1)
      (cond
         ((null? l1) '())
         (else (let ((pivot (car l1)))
            (append (append (quicksort (partition (lambda (x) (< x pivot)) l1))
                       (partition (lambda (x) (= x pivot)) l1))
                    (quicksort (partition (lambda (x) (> x pivot)) l1)))))))
