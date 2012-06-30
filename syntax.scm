(use srfi-1)

(lazy-defmacro 'let1
  (lambda (var expr body)
    `((lambda (,var) ,body) ,expr)))

(lazy-defmacro 'let*
  (lambda (binds body)
    (let rec ((binds binds))
      (if (null? binds)
	  body
	  `(let1 ,(caar binds) ,(cadar binds)
		 ,(rec (cdr binds)))))))

(lazy-defmacro 'let
  (lambda (binds body)
    (let ((vars (map car binds))
	  (exprs (map cadr binds)))
      `((lambda ,vars ,body) ,@exprs))))

; (incomplete-list a b c) => (cons a (cons b (cons c ???)))
(lazy-defmacro 'incomplete-list
  (lambda args
    (let rec ((args args))
      (if (null? (cdr args))
	  `(lambda (f) (f ,(car args) f))
	  `(cons ,(car args) ,(rec (cdr args)))))))

; (cons* a b c) => (cons a (cons b c))
(lazy-defmacro 'cons*
  (lambda args
    (let rec ((args args))
      (if (null? (cddr args))
	  `(cons ,(car args) ,(cdr args))
	  `(cons ,(car args) ,(rec (cdr args)))))))

; (o* f g h) => (lambda (x) (f (g (h x))))
(lazy-defmacro 'o*
  (lambda args
    `(lambda (x) ,(fold-right list 'x args))))

(lazy-defmacro 'list
  (lambda es
    (fold-right (lambda (hd tl) `(cons ,hd ,tl))
		'()
		es)))
