#!/usr/local/bin/gosh
(use srfi-13)

(load "./lazier.scm")
(load "syntax.scm")
(load "optimize.scm")
(load "prelude.scm")
(load "churchnum.scm")

(define dur-unit 64)
(define sampling-rate 8000)
(define initial-octave 0)
(define initial-deflen (/ dur-unit 4))
(define initial-volume 15)
(define *tempo* 120)

;; MML parser

(define (tokenize str)
  (cond ((rxmatch #/^([<>]|[ov]\d+|l\d+\.?|[a-gr][-+]?\d*\.?)/ str)
         => (lambda (m)
              (cons (m 0) (tokenize (rxmatch-after m)))))
        ((string-null? str) '())
        (else
         (error "parse error: " str))))

(define (string-cdr s)
  (string-copy s 1))

(define (parse-part octave deflen volume tokens)
  (if (null? tokens)
      '()
      (let ((tok (car tokens))
            (tokens (cdr tokens)))
        (case (string-ref tok 0)
          ((#\>)
           (parse-part (+ octave 1) deflen volume tokens))
          ((#\<)
           (parse-part (- octave 1) deflen volume tokens))
          ((#\o)
           (parse-part (string->number (string-cdr tok))
                       deflen volume tokens))
          ((#\v)
           (parse-part octave deflen
                       (string->number (string-cdr tok)) tokens))
          ((#\l)
           (parse-part octave (parse-length (string-cdr tok)) volume
                       tokens))
          ((#\r)
           (cons (make-rest (parse-length (string-cdr tok) deflen))
                 (parse-part octave deflen volume tokens)))
          ((#\a #\b #\c #\d #\e #\f #\g)
           (cons (parse-note tok octave deflen volume)
                 (parse-part octave deflen volume tokens)))))))

(define (parse-length s :optional default)
  (rxmatch-let (rxmatch #/^(\d*)(\.)?$/ s) (#f n dot)
    (let* ((n (string->number n))
           (len (if n (/ dur-unit n) default)))
      (if dot (/ (* 3 len) 2) len))))

(define (parse-note s octave deflen volume)
  (let1 t (tone (string-ref s 0))
    (case (string-ref s 1 #f)
      ((#\+)
       (make-note (+ t 1 (* 12 octave))
                  (parse-length (string-copy s 2) deflen)
                  volume))
      ((#\-)
       (make-note (+ t -1 (* 12 octave))
                  (parse-length (string-copy s 2) deflen)
                  volume))
      (else
       (make-note (+ t (* 12 octave))
                  (parse-length (string-copy s 1) deflen)
                  volume)))))

(define (tone ch)
  (case ch
    ((#\c) -9)
    ((#\d) -7)
    ((#\e) -5)
    ((#\f) -4)
    ((#\g) -2)
    ((#\a) 0)
    ((#\b) 2)))

;; Music data generator

(define (freq-inv pitch)
  (x->integer (/ sampling-rate (* 440 (expt 2 (/ pitch 12))))))

(define (unit-length tempo)
  (x->integer (/ (* sampling-rate 240) (* tempo dur-unit))))

(define (make-rest dur)
  (list 'R dur))

(define (make-note pitch dur vol)
  (list pitch dur vol))

(define (generate-play-data part)
  `(list
    ,@(map (lambda (x)
             (if (eq? (car x) 'R)
                 `(note 1 I
                        ,(cadr x)
                        0)
                 (let1 f (freq-inv (car x))
                   `(note ,(quotient f 2)
                          ,(if (odd? f) 'succ 'I)
                          ,(cadr x)
                          ,(caddr x)))))
           part)
    (note 1 I 1 256)))


;; Lazy K functions

(define (fold1 f xs)
  (if (null? (cdr xs))
      (car xs)
      (f (car xs) (fold1 f (cdr xs)))))

(define (define-lazyk-functions parts)
  (lazy-def '(square vol n parity)
    '(Y (lambda (x)
          (n (cons vol)
             (parity n (cons 0) x)))))

  (lazy-def '(take rest n)
    '((* n unit-duration)
      (lambda (g lst)
        (lst (lambda (hd tl)
               (S (S I (K hd)) (K (g tl))))))  ; (cons hd (g tl))
      (K rest)))

  (lazy-def '(note t parity dur vol)
    '(lambda (f) (f t parity dur vol)))

  (lazy-def 'play-part
    '(Y
      (lambda (rec music)
        (music
         (lambda (hd tl)
           (hd
            (lambda (t parity dur vol)
              (take (rec tl) dur (square vol t parity)))))))))

  (lazy-def 'mix-parts
    '((lambda (x) (x x))
      (lambda (rec xs ys)
        (xs (lambda (xhd xtl)
              (ys (lambda (yhd ytl)
                    (cons (+ xhd yhd)
                          (rec rec xtl ytl)))))))))

  (lazy-def '(main _)
    `(let ((play play-part)
           (mix mix-parts))
       ,(fold1 (lambda (x y) `(mix ,x ,y))
               (map (lambda (part)
                      `(play ,(generate-play-data part)))
                    parts))))
  )


;; main

(define (mml2lazy port)
  (let ((lines (map (lambda (s)
                      (string-delete (string-downcase s) char-whitespace?))
                    (port->list read-line port))))
    (if (rxmatch #/^t\d+$/ (car lines))
        (begin
          (set! *tempo* (string->number (string-cdr (car lines))))
          (set! lines (cdr lines))))
    (lazy-def 'unit-duration (unit-length *tempo*))
    (let ((parts (remove
                  null?
                  (map (lambda (line)
                         (parse-part initial-octave initial-deflen initial-volume
                                     (tokenize line)))
                       lines))))
;      (display (map generate-play-data parts)) (newline)
      (define-lazyk-functions parts)
      (print-as-unlambda (laze 'main)))))

(define (main args)
  (if (null? (cdr args))
      (mml2lazy (current-input-port))
      (call-with-input-file (cadr args)
        (lambda (p)
          (mml2lazy p)))))
