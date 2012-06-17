#!/usr/local/bin/gosh
(use gauche.record)
(load "lazier.scm")
(load "syntax.scm")
(load "optimize.scm")
(load "prelude.scm")
(load "churchnum.scm")

(define dur-unit 64)
(define sampling-rate 8000)

(define (tokenize str)
  (cond ((rxmatch #/^([<>]|[ov]\d+|l\d+\.?|[a-gr][-+]?\d*\.?)/ str)
         => (lambda (m)
              (cons (m 0) (tokenize (rxmatch-after m)))))
        ((> (string-length str) 0)
         (error "parse error: " str))
        (else '())))

(define (string-cdr s)
  (string-copy s 1))

(define (parse-part octave deflen volume tokens)
  (if (null? tokens)
      '()
      (ecase (string-ref (car tokens) 0)
        ((#\>)
         (parse-part (+ octave 1) deflen volume (cdr tokens)))
        ((#\<)
         (parse-part (- octave 1) deflen volume (cdr tokens)))
        ((#\o)
         (parse-part (string->number (string-cdr (car tokens)))
                     deflen volume (cdr tokens)))
        ((#\v)
         (parse-part octave deflen
                     (string->number (string-cdr (car tokens))) (cdr tokens)))
        ((#\l)
         (parse-part octave (parse-length (string-cdr (car tokens))) volume
                     (cdr tokens)))
        ((#\r)
         (cons (make-rest (parse-length (string-cdr (car tokens)) deflen))
               (parse-part octave deflen volume (cdr tokens))))
        ((#\a #\b #\c #\d #\e #\f #\g)
         (cons (parse-note (car tokens) octave deflen volume)
               (parse-part octave deflen volume (cdr tokens)))))))

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
  (ecase ch
    ((#\c) -9)
    ((#\d) -7)
    ((#\e) -5)
    ((#\f) -4)
    ((#\g) -2)
    ((#\a) 0)
    ((#\b) 2)))

(define (freq-inv pitch)
  (x->integer (/ sampling-rate (* 2 440 (expt 2 (/ pitch 12))))))

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
                 `(note 1
                        ,(cadr x)
                        0)
                 `(note ,(freq-inv (car x))
                        ,(cadr x)
                        ,(caddr x))))
           part)
    (note 1 1 256)))

(define mml "l8cdefedcrefgagferc4c4c4c4l16ccddeeffl8edc4r1")
;(define mml "v13l4r1.gf+ef+ef+g2gf+ef+ede2gf+ef+eg.r1r1r4.gf+ef+ef+g2gf+ef+ede2gf+ef+eg2l1rrrrl8r<ereeereeereerf4l1rrrrrrl8rereeereeereerf4l1rrrrrrrrr2.>l4gf+ef+ef+g2gf+ef+ede2gf+ef+eg.r1r1r4.gf+ef+ef+g2gf+ef+ede2")
(define part (parse-part 0 (/ dur-unit 4) 16 (tokenize mml)))
;(display (generate-play-data part))

(define *tempo* 183)
(lazy-def 'unit-duration (unit-length *tempo*))

(lazy-def '(square vol n)
  '(Y (lambda (x)
        (n (cons vol)
           (n (cons 0) x)))))

(lazy-def '(take rest n)
  '((* n unit-duration)
    (lambda (g lst)
      (lst (lambda (hd tl)
             (S (S I (K hd)) (K (g tl))))))  ; (cons hd (g tl))
    (K rest)))

(lazy-def '(note t dur vol)
  '(lambda (f) (f t dur vol)))

(lazy-def 'play
  '(Y
    (lambda (rec music)
      (music
       (lambda (hd tl)
         (hd
          (lambda (t dur vol)
            (take (rec tl) dur (square vol t)))))))))

(lazy-def 'music
          (generate-play-data part))

(lazy-def '(main _)
  '(play music))

(print-as-unlambda (optimize (laze 'main)))
