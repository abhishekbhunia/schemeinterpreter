

(define (repl)     ;;; the read-eval-print loop. 
  (display "--++> ") 
  (let ((exp (read)))
    (cond ((equal? exp '(exit))      ; (exit) only allowed at top level
	   'done)
	  (else  (display (top-eval exp))
		 (newline)
		 (repl))
	  )))

;;LOAD  
(define (my-load filename)        
  (load-repl (open-input-file filename)))


(define (load-repl port)
  (let ((exp (read port)))
    (cond ((eof-object? exp) 'done)
	  (else (let ((res (top-eval exp)))
		  (display res)
		  (load-repl port)))
	  )))

;;TOP LEVEL EVAL 
(define (top-eval exp)
  		(cond ((not (pair? exp)) 
  					(my-eval exp *global-env*)
  			  )
       		  ((eq? (car exp) 'define)        
          			(insert! (list (find-name exp) (my-eval (find-mapping exp) *global-env*)) *global-env*)
                    (find-name exp)
         	  )
        	  (else (my-eval exp *global-env*))
        )
)

(define (find-name exp)
  (cond ((symbol? (cadr exp)) (cadr exp))
        (else (car(cadr exp)))))


(define (find-mapping exp)
  (cond ((symbol? (cadr exp)) (cadr (cdr exp)))
        (else (cons 'lambda (cons (cdr(cadr exp)) 
                      (cddr exp))))))

;;2ND LEVEL EVAL  
(define (my-eval exp env)
  (cond
   ((symbol? exp) (lookup exp env))
   ((not (pair? exp)) exp)
   ((eq? (car exp) 'quote) (cadr exp))
   ((eq? (car exp) 'if)
   	(handle-if (cadr exp) (car (cddr exp)) (cadr (cddr exp)) env))
   ((eq? (car exp) 'lambda)
    (list 'closure exp env))
   ((eq? (car exp) 'letrec)
    (handle-letrec (cdr exp) env))   
   ((eq? (car exp) 'let)
    (handle-let exp env)) 
   ((eq? (car exp) 'let*)
    (handle-let* (cadr exp) (cddr exp) env)) 
   ((eq? (car exp) 'begin)
    (handle-begin exp env))
   ((eq? (car exp) 'cond)
    (handle-cond (cdr exp) env))
   ((eq? (car exp) 'not) 
    (handle-not (cadr exp) env))
   (else
    (handle-call (map (lambda (sub-exp) (my-eval sub-exp env)) exp)))
  )
)

;;INSERT  
(define (insert! val L)
  (set-cdr! L (cons (car L) (cdr L)))
  (set-car! L val)
  )

;;LOOKUP  
(define (lookup var env)
  (let ((item (assoc var env)))  
    (cond ((not item) (display "Error: Undefined Symbol ")
		      (display var) (newline))
	  (else (cadr item))
	  )))

;;HANDLE IF  
(define (handle-if test then-exp else-exp env)
  (if (my-eval test env)
      (my-eval then-exp env)
      (my-eval else-exp env)))

;;BIND 
(define (bind formals actuals);; 
  (cond ((null? formals) '())
	(else (cons (list (car formals) (car actuals))
		    (bind (cdr formals) (cdr actuals))))
	))

;;HANDLE CALL  
(define (handle-call evald-exps)
  (let ((fn (car evald-exps))
	(args (cdr evald-exps)))
   (cond
    ((eq? (car fn) 'closure)
     (let ((formals (cadr (cadr fn)))
	   (body (cddr (cadr fn)))
	   (env (caddr fn)))
	   (handle-block body (append (bind formals args) env))))
    ((eq? (car fn) 'primitive-function)
     (apply (cadr fn) args))
    (else (display "Error: HERE Calling non-function"))
    )))



;;HANDLE-LET
(define (handle-let let-form env)

  (let ((let-bindings (cadr let-form))
        (let-body (cddr let-form)))
        
   
      (let ((var-list (map car let-bindings))
            (init-val-exprs (map cadr let-bindings)))

         
        (let ((new-env (append (bind var-list (eval-multi init-val-exprs env)) env)))
           
            (handle-block let-body new-env))))
)

;;EVAL MULTI
(define (eval-multi multi-exp env)
   (map (lambda (x)
           (my-eval x env))
        multi-exp)
)

;;HANDLE-BLOCK  
(define (handle-block block env)
  (cond ((null? block) (display "Error: Can't have empty block or body"))
	((null? (cdr block)) (my-eval (car block) env))
	(else (my-eval (car block) env)
	      (handle-block (cdr block) env))
	)
)

;;HANDLE-BEGIN
(define (handle-begin exp env)
 	(cond((null? (cddr exp)) (my-eval (cadr exp) env))
 		(else (my-eval (cadr exp) env)
 			  (handle-begin (cddr exp) env))
 	)
)

;;HANDLE-LET*
(define (handle-let* let*-def let*-body env)
  (let* ((new-env (get-env let*-def env)))
    (handle-block let*-body new-env)
  )
)
(define (get-env def env)
  (if (null? def) env
    (update-env def env)
  )
)
(define (update-env def env)
  (insert! (list (caar def) (handle-block (cdr (car def)) env)) env)
  (get-env (cdr def) env)
)

;;HANDLE-LETREC
(define (handle-letrec exp env)
   (let* ((vars (map car (car exp)))
        (exprs (map cadr (car exp))) 
        (letrec-body (cdr exp))
        (garbage (map (lambda (x) '*uninitialized*) exprs))
        (new-env (append (bind vars garbage) env))
        (vals (eval-multi exprs new-env)))
      (change-env! vals new-env) 
      (handle-block letrec-body (append (bind vars vals) env))
    )
)
(define (change-env! vals env)   
        (cond ((null? vals) '())
              (else (set-car! (cdr (car env)) (car vals))(change-env! (cdr vals) (cdr env)))
	    )
)
		
;;HANDLE-COND
(define (handle-cond conds env)
   (cond ((null? conds) '())
         ((eq? (caar conds) 'else)
          (handle-block (cdr (car conds)) env))
         ((eq? #f (my-eval (caar conds) env))
          (handle-cond (cdr conds) env))
         (else 
          (handle-block (cdr (car conds)) env))
    )
)

;;HANDLE-NOT
(define (handle-not exp env)
   (cond ((eq? #f (my-eval exp env)) #t)
         (else #f))
)

;;MY-APPLY
(define (my-apply fun arg)
  (cond ((eq? (car fun) 'primitive-function)
         (apply (cadr fun) arg))
        ((eq? (car fun) 'closure)
         (handle-call (cons fun arg)))
        (else (display "error in my-apply"))
   )
)

;;-------------------- global environment --------

(define *global-env*
  (list (list 'car (list 'primitive-function car))
	(list 'cdr (list 'primitive-function cdr))
	(list 'set-car! (list 'primitive-function set-car!))
	(list 'set-cdr! (list 'primitive-function set-cdr!))
	(list 'cons (list 'primitive-function cons))
	(list 'list (list 'primitive-function list))	
	(list '+ (list 'primitive-function +))
	(list '- (list 'primitive-function -))
	(list '* (list 'primitive-function *))
	(list '= (list 'primitive-function =))
	(list '< (list 'primitive-function <))
	(list '> (list 'primitive-function >))
	(list '<= (list 'primitive-function  <=))
	(list '>= (list 'primitive-function >=))
	(list 'eq? (list 'primitive-function eq?))
	(list 'pair? (list 'primitive-function pair?))
	(list 'symbol? (list 'primitive-function symbol?))
	(list 'null? (list 'primitive-function null?))
	(list 'read (list 'primitive-function read))
	(list 'display (list 'primitive-function  display))
	(list 'open-input-file (list 'primitive-function open-input-file))
	(list 'close-input-port (list 'primitive-function close-input-port))
	(list 'eof-object? (list 'primitive-function eof-object?))
	(list 'load (list 'primitive-function my-load))   
	(list 'newline (list 'primitive-function newline))
	(list 'apply (list 'primitive-function my-apply))
	))


