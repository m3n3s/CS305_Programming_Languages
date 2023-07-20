; CS305 HW5
; Mehmet Enes Battal - 26354

(define s7-interpret (lambda (e env)
	(cond 
		((number? e) e)

		((symbol? e) 
			(get-value e env)
		)

		((if-stmt? e) 
			(if (equal? (s7-interpret (cadr e) env) 0)
				(s7-interpret (cadddr e) env)
				(s7-interpret (caddr e) env)
			)
		)
				
		((cond-stmt? e)
			(if(not(equal? (s7-interpret (caadr e) env) 0))
				(s7-interpret (cadadr e) env)
				(s7-interpret (cons 'cond (cddr e)) env )
			)
		)

		((cond-else? e)
			(s7-interpret(cadadr e) env )
		)

		((let-stmt? e) 
			(let ((vars (map car (cadr e))) (initvals (map cadr (cadr e))))
				(let ((vals (map (lambda (n) (s7-interpret n env)) initvals)))
					(let ((new-env (append (map cons vars vals) env)))
						(s7-interpret (caddr e) new-env)
					)
				)
			)
		)

		((letstar-stmt? e) 
			(let ((new-env (extend-env (caaadr e) (s7-interpret (car (cdaadr e)) env) env)))
				(if (> (length (cadr e)) 1)
					(s7-interpret (list (car e) (cdadr e) (caddr e)) new-env)
					(s7-interpret (caddr e) new-env)
				)
			)
		)

		((not (list? e)) "ERROR")

		((operator? (car e))
			(let ((operands (map s7-interpret (cdr e) (make-list (length (cdr e)) env)))
				(operator 
					(cond 
						((and (equal? (car e) '-) (< (length e) 3)) "ERROR")
						((and (equal? (car e) '/) (< (length e) 3)) "ERROR")
						(else (get-operator (car e)))
					)
				))
				(apply operator operands)
			)
		)
		
		(else "ERROR")
	)
))

(define if-stmt? (lambda (e)
	(and (list? e) (equal? (car e) 'if) (= (length e) 4))
))

(define cond-stmt? (lambda (e)
	(and 
		(equal? (car e) 'cond)
		(> (length e) 2)
		(list? (cadr e))
		(last-else? e) 
		(only-else? (cdr e))
	)
))

(define cond-else? (lambda (e)
	(and 
		(equal? (length e) 2) 
		(list? (cadr e)) 
		(equal? (caadr e) 'else )
	)
))

(define last-else? (lambda (e)
	(equal? (caar (reverse e)) 'else))
)

(define only-else? (lambda (e)
	(if(and (equal? (caar e) 'else ) (null? (cdr e)))
		#t
		(if(and (equal? (caar e) 'else ) (not(null? (cdr e))) )
			#f
			(only-else? (cdr e))
		)
	)
))

(define let-stmt? (lambda (e)
	(and 
		(list? e) 
		(equal? (car e) 'let) 
		(= (length e) 3)
		(or (equal? () (cadr e) ) (valid-list? (cadr e)))
	)
))

(define letstar-stmt? (lambda (e)
	(and (list? e) (equal? (car e) 'let*) (= (length e) 3))
))

(define valid-list? (lambda (e)
    (and 
		(list? e) 
		(list? (car e)) 
		(= (length (car e)) 2) 
		(symbol? (caar e))
        (or (null? (cdr e)) (valid-list? (cdr e)))
	)
))

(define var-binding-list? (lambda (e)
	(cond
		((and (list? e) (symbol? (caar e) (= (length (car e) 2))) (not (null? (cdr e)))) (var-binding-list? (cdr e)))
		((and (list? e) (symbol? (caar e) (= (length (car e) 2))) ((null? (cdr e)))) #t)
		(else "ERROR")
	)
))

(define operator? (lambda (op-symbol)
	(cond
		((equal? op-symbol '+) #t)
		((equal? op-symbol '-) #t)
		((equal? op-symbol '*) #t)
		((equal? op-symbol '/) #t)
		(else #f)
	)
))

(define get-operator (lambda (op-symbol) 
	(cond 
		((equal? op-symbol '+) +)
		((equal? op-symbol '-) -)
		((equal? op-symbol '*) *)
		((equal? op-symbol '/) /)
		(else "ERROR")
	)
))

(define get-value (lambda (var env)
    (cond
		((null? env) "ERROR")
		((equal? (caar env) var) (cdar env))
		(else (get-value var (cdr env)))
	)
))

(define extend-env (lambda (var val old-env)
      (cons (cons var val) old-env)
))

(define operand-list? (lambda (e)
	(cond
		((and (list? e) (operand-list? (cdr))) #t)
		((null? e) #t)
		(else #f)
	)
))

(define define-stmt? (lambda (e)
	(and (list? e) (equal? (car e) 'define) (symbol? (cadr e)) (= (length e) 3))
))

(define repl (lambda (env)
	(let* (
		(dummy1 (display "cs305> "))
		(expr (read))
		(new-env 
			(if (define-stmt? expr)
				(extend-env (cadr expr) (s7-interpret (caddr expr) env) env)
				env
			)
		)
		(val (if (define-stmt? expr)
				(cadr expr)
				(s7-interpret expr env)
			)
		)
		(dummy2 (display "cs305: "))
		(dummy3 (display val))

		(dummy4 (newline))
		(dummy4 (newline))
	) (repl new-env))
))

(define cs305 (lambda () (repl '())))