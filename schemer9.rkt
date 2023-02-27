#lang racket
; Funções intermediárias

(define pick
  (lambda (n lat)
  (cond
    ((eq? n 1) (car lat))
    (else (pick (sub1 n) (cdr lat))))))

(pick '1 '(lasagna spaghetti ravioli macaroni meatball))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))


; Minha tentativa de keep-looking
; Na minha versão, o caviar é encontrado...
;(define keep-looking
;  (lambda (a word lat)
;    (cond
;      ((eq? a word) #t)
;      ((null? lat) #f)
;      (else (looking a (cdr lat))))))
;
;(define looking
;  (lambda (a lat)
;    (keep-looking a (pick 1 lat) lat)))
;
;(looking 'caviar '(6 2 4 caviar 5 7 3))
;(looking 'caviar '(6 2 grits caviar 5 7 3))

; Só que a versão do autor tem um objetivo completamente diferente... A ideia
; é usar o resultado para continuar procurando. Tentei definir aqui do meu jeito.

;(define keep-looking
;  (lambda (a pos lat)
;    (cond
;      ((eq? a (pick pos lat)) #t)
;      (else (keep-looking a (pick pos lat) lat)))))

; O problema da minha versão é que se não for um número, como o pick vai funcionar?
; Ajustando!
(define keep-looking
  (lambda (a pos lat)
    (cond
      ((number? pos) (keep-looking a (pick pos lat) lat))
      (else (eq? a pos)))))

(keep-looking 'caviar (pick 1 '(6 2 4 caviar 5 7 3)) '(6 2 4 caviar 5 7 3))

; Definindo shift

(define shift
  (lambda (pair)
    (build (first (first pair)) (build (second (first pair)) (second pair)))))

(shift '((a b) (c d)))

; Escrevendo a função align
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora)) (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (weight* (first pora)) 2) (weight* (second pora)))))))

(weight* '((a b) c))
(weight* (shift '((a b) c)))

; definindo shuffle

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

(shuffle '(a (b c)))
(shuffle '(a b))
;(shuffle '((a b) (c d))) ; Cai na segunda condição eternamente!

; A função Ackermann
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))

(A 1 0)
(A 2 0)
;(A 4 3) ;  Roda eternamente!

; O pedaço do Y combinator

(define eternity
  (lambda (x)
    (eternity x)))

(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l)))))) ; Não dá resposta!

(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (l) (cond ((null? l) 0) (else (add1 (eternity (cdr l)))))) (cdr l))))))


(define Y
  (lambda (le)
    ((lambda (f) (f f)))
    (lambda (f)
      (le (lambda (x) ((f f) x))))))