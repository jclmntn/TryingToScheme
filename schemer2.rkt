#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x )))))

; Definição de member Little Schemer
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f ))))

; Minha definição de member?
;(define member?
;  (lambda (a l)
;    (cond
;      ((null? l) #f)
;      ((eq? a (car l)) #t)
;      (else (member? a (cdr l))))))

; Como podemos simplificar a versão anterior de member usando uma função primitiva?
; Será que poderíamos usar a primitiva or para simplificá-la?

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))


(define lat '(coffee tea or milk))
(member? 'tea lat)