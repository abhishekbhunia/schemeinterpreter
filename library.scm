;;Scheme Interpreter Library


(define (caar exp)
  (car (car exp)))

(define (cadr exp) 
  (car (cdr exp)))

(define (cddr exp)
  (cdr (cdr exp)))

(define (caddr exp)
  (car (cdr (cdr exp))))
  
(define (and x y)
  (if x y #f))

(define (or x y)
  (if x #t y))

(define (assoc var env)
  (cond ((null? env) #f)
        ((eq? var (car (car env))) (car env))
        (else (assoc var (cdr env)))
   )
)

(define (append L1 L2)
  (cond ((null? L1) L2)
        (else (cons (car L1)  (append (cdr L1) L2)))
   )
)

(define (equal? L1 L2)
  (cond ((and (pair? L1) (pair? L2)) 
         (and (equal? (car L1) (car L2))(equal? (cdr L1) (cdr L2)))
		)
        (else (eq? L1 L2))
   )
)

(define (map f L)
   (cond ((null? L) '())
         (else (cons (f (car L)) (map f (cdr L))))
   )
)

