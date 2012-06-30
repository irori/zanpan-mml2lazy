#!/usr/local/bin/gosh
;; MML -> LazyK converter
;; Copyright 2012 irori <irorin@gmail.com>
;;
;; usage:
;;   mml2lazy.scm hoge.mml > hoge.lazy
;;
;; MML Syntax:
;;
;; <MML> := <tempo>?<part>(\n<part>)*
;; <tempo> := t<digits>                -- テンポ (BPM) 初期値は120
;; <part> := <elem>*
;; <elem> := [cdefgab][+-]?<length>?   -- 音符
;;         | r<length>?                -- 休符
;;         | l<length>                 -- デフォルトの音長指定。初期値は4
;;         | o<digits>                 -- オクターブ変更。初期値は4
;;         | '>'                       -- オクターブ 1 上げる
;;         | '<'                       -- オクターブ 1 下げる
;;         | v<digits>                 -- ボリューム変更。初期値は16
;; <length> := (1|2|4|8|16|32).?       -- 音の長さ。.をつけると3/2倍
;; <digits> := [0-9]+

(use srfi-13)

(load "./lazier.scm")
(load "./syntax.scm")
(load "./prelude.scm")
(load "./churchnum.scm")

(define dur-unit 64)
(define sampling-rate 8000)
(define initial-octave 0)
(define initial-deflen (/ dur-unit 4))
(define initial-volume 16)
(define *tempo* 120)

;; MML parser

;; MML をトークンに分解する
(define (tokenize str)
  (cond ((rxmatch #/^([<>]|[ov]\d+|l\d+\.?|[a-gr][-+]?\d*\.?)/ str)
         => (lambda (m)
              (cons (m 0) (tokenize (rxmatch-after m)))))
        ((string-null? str) '())
        (else
         (error "parse error: " str))))

(define (string-cdr s)
  (string-copy s 1))

;; 1 つのパートをパースする
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
           (parse-part (- (string->number (string-cdr tok)) 4)
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

(define (part-duration part)
  (if (null? part)
      0
      (+ (cadar part) (part-duration (cdr part)))))

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

(define (generate-play-data2 part)
  (compress-play-data
   (append
    (map
     (lambda (x)
       (if (eq? (car x) 'R)
	   (list 1 'I (cadr x) 0)
	   (let1 f (freq-inv (car x))
	     (list (quotient f 2)
		   (if (odd? f) 'succ 'I)
		   (cadr x)
		   (caddr x)))))
     part)
    '((1 I 1 256)))))

(define (compress-play-data part)
  (if (null? part)
      '()
      (let ((rest (compress-play-data (cdr part)))
	    (i (list-index (lambda (x) (equal? (car part) x)) (cdr part))))
	(cond ((not i)
	       `(cons ,(cons 'note (car part))
		      ,rest))
	      ((= i 0) `(ref0 ,rest))
	      (else `(ref ,i ,rest))))))

;; Lazy K functions

(define (define-lazyk-functions parts)
  ;; 音符 1 つのデータ
  ;; 周波数 (1 / 2t+parity), 長さ dur, 音量 vol
  ;; note :: Int -> Int -> Int -> Int -> Note
  (lazy-def '(note t parity dur vol)
    '(lambda (f) (f t parity dur vol)))

  ;; 楽譜データ圧縮用
  (lazy-def '(ref n xs)
    '(cons (nth n xs) xs))
  (lazy-def '(ref0 xs)
    '(cons (car xs) xs))

  ;; square :: Int -> Int -> Int -> [Int]
  ;; 周期 (2n + parity), 振幅 vol の矩形波を生成する
  (lazy-def '(square vol n parity)
    '(Y (lambda (x)
          (n (cons vol)
             (parity n (cons 0) x)))))

  ;; take :: [a] -> Int -> [a] -> [a]
  ;; take ys n xs は xs の先頭要素 n 個と ys を連結したものを返す
  (lazy-def '(take rest n)
    '((* n unit-duration)
      (lambda (g lst)
        (lst (lambda (hd tl)
               (S (S I (K hd)) (K (g tl))))))  ; (cons hd (g tl))
      (K rest)))

  ;; 1 つのパートの波形を生成する
  ;; play-part :: [Note] -> [Int]
  (lazy-def 'play-part
    '(Y
      (lambda (rec music)
        (music
         (lambda (hd tl)
           (hd
            (lambda (t parity dur vol)
              (take (rec tl) dur (square vol t parity)))))))))

  ;; 2 つの波形データを合成する
  ;; mix-parts :: [Int] -> [Int] -> [Int]
  (lazy-def 'mix-parts
    '((lambda (x) (x x))
      (lambda (rec xs ys)
        (xs (lambda (xhd xtl)
              (ys (lambda (yhd ytl)
                    (cons (+ xhd yhd)
                          (rec rec xtl ytl)))))))))

  ;; エントリポイント
  (lazy-def '(main _)
    `(let ((play play-part)
           (mix mix-parts))
       ,(fold1 (lambda (x y) `(mix ,x ,y))
               (map (lambda (part)
                      `(play ,(generate-play-data2 part)))
                    parts))))
  )

(define (fold1 f xs)
  (if (null? (cdr xs))
      (car xs)
      (f (car xs) (fold1 f (cdr xs)))))

;; main

(define *dump-score* #f)

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
      (if *dump-score*
	  (for-each
	   (lambda (part)
	     (display "(n")
	     (for-each (lambda (note) (display note) (newline))
		       (generate-play-data part))
	     (display ")\n")
	     )
	   parts)
	  (begin
	    (define-lazyk-functions parts)
	    (print-as-unlambda (laze 'main)))))))

(define (main args)
  (if (and (not (null? (cdr args))) (equal? (cadr args) "-d"))
      (begin
	(set! *dump-score* #t)
	(set! args (cdr args))))
  (if (null? (cdr args))
      (mml2lazy (current-input-port))
      (call-with-input-file (cadr args)
        (lambda (p)
          (mml2lazy p)))))
