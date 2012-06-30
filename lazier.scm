;;
;; Lazier, a "compiler" from lambda calculus to Lazy K.
;; Copyright 2002 Ben Rudiak-Gould. Distributed under the GPL.
;;
;; Usage examples:
;;
;; > (lazy-def '(myprog input) '(cdr (cdr input))) ; drops first two bytes of input
;; > (lazy-def 'myprog '(o cdr cdr))	; equivalent definition
;;
;; > (laze 'myprog)
;; ((s ((s i) (k (k i)))) (k (k i)))
;;
;; > (print-as-cc (laze 'myprog))
;; S(SI(K(KI)))(K(KI))
;;
;; > (print-as-unlambda (laze 'myprog))
;; ``s``si`k`ki`k`ki
;;
;; > (print-as-iota (laze 'myprog))
;; ***i*i*i*ii***i*i*i*ii*ii**i*i*ii**i*i*ii*ii**i*i*ii**i*i*ii*ii
;;
;; > (print-as-jot (laze 'myprog))
;; 111111100011111110001111111110000011110011110011111111100000111100111100
;; 11111111100000
;;
;; > (lazy-def '(f x y z) '(x (y z)))			; \
;; > (lazy-def 'f '(lambda (x y z) '(x (y z))))		; | equivalent
;; > (lazy-def 'f '(lambda (x) (lambda (y) ...)))	; /
;;
;; > (laze '(f arg1 arg2))
;; ((s (k arg1)) arg2)
;;
;; > (print-as-unlambda (laze '(f arg1 arg2)))
;; ``s`k[arg1][arg2]
;;

; lazy-defmacro.
(define lazy-macros '())

(define (lazy-defmacro name body)
  (set! lazy-macros
	(cons (cons name body) lazy-macros)))

(define (lazy-macro-lookup name)
  (assv name lazy-macros))

(define (uncurried-expr-dispatch expr leaf appl lamb)
  (if (pair? expr)
      (if (eq? (car expr) 'lambda)
          (lamb (cadr expr) (caddr expr))
          (apply appl expr))
      (leaf expr)))

(define (expand-macros expr)
  (let helper ((expr expr) (exclude '()) (stack '()))
    (uncurried-expr-dispatch expr
      (lambda (leaf) leaf)
      (lambda (op . args)
	(cond ((memv op exclude)
	       (cons op (map (lambda (e) (helper e exclude stack))
			     args)))
	      ((memv op stack)
	       (display "Recursion within lazy-macros detected: ")
	       (display (cons op stack))
	       (newline)
	       (error))
	      (else
	       (let ((macro (lazy-macro-lookup op))
		     (args (map (lambda (e) (helper e exclude stack))
				args)))
		 (if macro
		     (helper (apply (cdr macro) args) exclude (cons op stack))
		     (cons (helper op exclude stack) args))))))
      (lambda (vars body)
	`(lambda ,vars ,(helper body (append vars exclude) stack))))))

; lazy-def.

(define lazy-defs '())

(define (eliminate-recursion name body)
  (if (contains-free-variable name body)
      `((lambda (x) (x x))
	(lambda (,name)
	  ,(var-subst name (list name name) body)))
      body))

(define (lazy-def name body)
  (let ((body (curry (expand-macros body))))
    (set! lazy-defs
	  (cons (if (pair? name)
		    (cons (car name)
			  (eliminate-recursion
			   (car name)
			   (curry-lambda (cdr name) (curry body))))
		    (cons name (eliminate-recursion name (curry body))))
		lazy-defs ))))

(define (lazy-def-lookup name)
  (assv name lazy-defs) )


; Currying.

(define (curry expr)
  (cond ((not (pair? expr)) expr)
        ((eq? (car expr) 'lambda)
         (curry-lambda (cadr expr) (curry (caddr expr))) )
        (else
         (curry-app (map curry expr)) )))

(define (curry-lambda vars body)
  (if (null? vars)
      body
      `(lambda (,(car vars)) ,(curry-lambda (cdr vars) body)) ))

(define (curry-app lst)
  (let iter ((sofar (car lst))
             (togo (cdr lst)) )
    (if (null? togo)
        sofar
        (iter (list sofar (car togo)) (cdr togo)) )))


; lazy-def expansion.

(define (expr-dispatch expr leaf appl lamb)
  (if (pair? expr)
      (if (eq? (car expr) 'lambda)
          (lamb (caadr expr) (caddr expr))
          (appl (car expr) (cadr expr)))
      (leaf expr)))

(define (expand-defs expr)
  (let helper ((expr expr) (exclude '()) (stack '()))
    (expr-dispatch expr
     (lambda (leaf)
       (cond ((memv leaf exclude) leaf)
             ((memv leaf stack)
              (display "Recursion within lazy-defs detected: ")
              (display (cons leaf stack))
              (newline)
              (error) )
             (else
              (let ((def (lazy-def-lookup leaf)))
                (if def
                    (helper (cdr def) exclude (cons leaf stack))
                    leaf )))))
     (lambda (f g)
       (list (helper f exclude stack) (helper g exclude stack)) )
     (lambda (var body)
       `(lambda (,var) ,(helper body (cons var exclude) stack)) ))))


; Replace ((lambda (var) body) value) with body[value/var] if:
;
;   - value is a symbol, or
;   - var appears only once in body and value contains no
;     more than one free variable which is not in body.
;
; I'm not sure if the first of these is ever needed -- it may
; always be handled by the other optimizations -- but it's easy
; to check for.

(define (apply-lambdas expr)
  (let ((top-level-free-vars (free-vars expr)))
    (let self ((expr expr))
      (expr-dispatch expr
       (lambda (leaf) leaf)
       (lambda (f g)
         (let ((f: (self f))
               (g: (self g)) )
           (expr-dispatch f:
            (lambda (leaf) (list f: g:))
            (lambda (f:: g::) (list f: g:))
            (lambda (var body)
              (if (or (not (pair? g:))
                      (and (<= (count-occurrences var body) 1)
                           (not (more-than-one-additional
                                 (free-vars g:)
                                 (append top-level-free-vars (free-vars f:)) ))))
                  (var-subst var g: body)
                  (list f: g:) )))))
       (lambda (var body)
         `(lambda (,var) ,(self body)) )))))

(define (add-prime var)
  (string->symbol (string-append (symbol->string var) ":")) )

(define (var-subst var value template)
  (if (eqv? var value)
      template
      (let loop ((template template))
        (expr-dispatch template
         (lambda (leaf)
           (if (eqv? var leaf) value leaf) )
         (lambda (f g)
           (list (loop f) (loop g)) )
         (lambda (v body)
           (if (eqv? var v)
               template
               (do ((template-vars (free-vars template))
                    (value-vars (free-vars value))
                    (v: v (add-prime v:)) )
                 ((and (not (memv v: template-vars))
                       (not (memv v: value-vars)) )
                  `(lambda (,v:)
                     ,(loop (var-subst v v: body)) )))))))))

(define (more-than-one-additional a b)
  (let loop ((a a) (last-sym (cons #f #f)))
    (cond ((null? a) #f)
          ((memv (car a) b)
           (loop (cdr a) last-sym) )
          ((or (pair? last-sym)		; no last symbol
               (eqv? last-sym (car a)) )
           (loop (cdr a) (car a)) )
          (else #t) )))

(define (free-vars-rec expr bound)
  (expr-dispatch expr
    (lambda (leaf)
      (if (memv leaf bound)
          ()
          (list leaf) ))
    (lambda (f g)
      (append (free-vars-rec f bound) (free-vars-rec g bound)) )
    (lambda (var body)
      (free-vars-rec body (cons var bound)) )))

(define *free-vars-cache* (make-hash-table 'equal?))
(define (free-vars expr)
  (or (hash-table-get *free-vars-cache* expr #f)
      (let1 result (free-vars-rec expr ())
        (hash-table-put! *free-vars-cache* expr result)
        result)))

(define (contains-free-variable param template)
  (expr-dispatch template
   (lambda (leaf)
     (eqv? param leaf) )
   (lambda (f g)
     (or (contains-free-variable param f)
         (contains-free-variable param g) ))
   (lambda (var body)
     (and (not (eqv? param var))
          (contains-free-variable param body) ))))

(define (count-occurrences param template)
  (expr-dispatch template
   (lambda (leaf)
     (if (eqv? param leaf) 1 0) )
   (lambda (f g)
     (+ (count-occurrences param f) (count-occurrences param g)) )
   (lambda (var body)
     (if (eqv? var param)
         0
        (count-occurrences param body) ))))


; Abstraction elimination.

(define (unabstract-lambda var body)
  (if (contains-free-variable var body)
      (expr-dispatch body
       (lambda (leaf) 'I)
       (lambda (f g)
         (if (and (eqv? var g) (not (contains-free-variable var f)))
             f
             `((S ,(unabstract-lambda var f)) ,(unabstract-lambda var g)) ))
       (lambda (v b)
         (unabstract-lambda var (unabstract body)) ))
      (list 'K body) ))

(define (unabstract code)
  (expr-dispatch code
   (lambda (leaf) leaf)
   (lambda (f g)
     (list (unabstract f) (unabstract g)) )
   (lambda (var body)
     (unabstract-lambda var (unabstract body)) )))


; Reduces expressions involving the S, K, I combinators where this
; results in a shorter expression. Usually results in only a small
; benefit.

(define (apply-ski expr)
  (if (not (pair? expr))
      expr
      (let ((lhs (apply-ski (car expr)))
            (rhs (cadr expr)) )
        (cond ((eq? lhs 'I)		; Ix -> x
               (apply-ski rhs) )
              ((and (pair? lhs)		; Kxy -> x
                    (eq? 'K (car lhs)) )
               (cadr lhs) )
              ((and (pair? lhs)		; Sxyz -> xz(yz) when x or y is K_
                    (pair? (car lhs))
                    (eq? 'S (caar lhs)) )
               (let ((z rhs)
                     (y (cadr lhs))
                     (x (cadar lhs)) )
                 (if (or (and (pair? x) (eq? (car x) 'K))
                         (and (pair? y) (eq? (car y) 'K)) )
                     (apply-ski `((,x ,z) (,y ,z)))
                     (list lhs (apply-ski rhs)) )))
              (else
               (list lhs (apply-ski rhs)) )))))


; This converts expressions of the form ((x z) (y z)) to (s x y z).
; If z is just a symbol, then this change makes no difference to
; Unlambda output, always reduces the size of CC output (I think),
; and can either increase or reduce the side of Iota and Jot output.
; Currently the change is made only when z is not just a symbol.
;
; Like apply-ski, this gives only a small benefit in most cases.

(define (unapply-s expr)
  (expr-dispatch expr
   (lambda (leaf) leaf)
   (lambda (f g)
     (let ((f: (unapply-s f))
           (g: (unapply-s g)) )
       (if (and (pair? f:)
                (pair? g:)
                (pair? (cadr f:))
                (equal? (cadr f:) (cadr g:)) )
           `(((S ,(car f:)) ,(car g:)) ,(cadr f:))
           (list f: g:) )))
   (lambda (var body)
     `(lambda (,var) ,(unapply-s body)) )))


; Putting it all together.

(define (laze code)
  (unapply-s
   (apply-ski
    (unabstract
     (apply-lambdas
      (expand-defs
       (curry
	(expand-macros
	 code))))))))


; Printing it out.

(define (print-as-cc lazified-code)
  (let self ((code lazified-code))
    (expr-dispatch code
     (lambda (leaf)
      (if (memq leaf '(I K S))
          (display (char-upcase (string-ref (symbol->string leaf) 0)))
          (begin
            (display "[")
            (display leaf)
            (display "]") )))
     (lambda (f g)
      (self f)
      (if (pair? g) (display "("))
      (self g)
      (if (pair? g) (display ")")) )
     (lambda (var body)
      (error "Can't print lambdas as CC!") )))
  (newline) )

(define (print-as-golf e)
  (if (pair? e)
      (begin (print-as-golf (car e)) (print-as-golf-unlambda (cadr e)))
      (display e)))
(define (print-as-golf-unlambda e)
  (if (pair? e)
      (if (pair? (car e))
	  (begin (display "(")
		 (print-as-golf e)
		 (display ")"))
	  (begin (display "`")
		 (print-as-golf-unlambda (car e))
		 (print-as-golf-unlambda (cadr e))))
      (display e)))

(define (print-as-generic aply k s i fvar)
  (lambda (lazified-code)
    (let self ((code lazified-code))
      (expr-dispatch code
       (lambda (leaf)
        (cond ((eq? leaf 'I) (display i))
              ((eq? leaf 'K) (display k))
              ((eq? leaf 'S) (display s))
              (else (fvar leaf)) ))
       (lambda (f g)
        (display aply)
        (self f)
        (self g) )
       (lambda (var body)
        (error "Can't print lambdas as Lazy code!") )))))

(define (default-print-fvar var)
  (display "[") (display var) (display "]"))

(define (combgolf-print-fvar var)
  (let ((str (symbol->string var)))
    (display (make-string (- (string-length str) 1) #\`))
    (display var)))

(define print-as-unlambda
  (print-as-generic "`" "k" "s" "i" default-print-fvar))
(define print-as-iota
  (print-as-generic "*" "*i*i*ii" "*i*i*i*ii" "*ii" default-print-fvar))
(define print-as-jot
  (print-as-generic "1" "11100" "11111000" "11111111100000" default-print-fvar))
(define print-as-combgolf
  (print-as-generic "`" "k" "s" "i" combgolf-print-fvar))
