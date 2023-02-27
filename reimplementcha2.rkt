#lang racket
; O primitivo primordial: verificar se um valor é um átomo!

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Reimplementando lat: uma função que verifica se todos os elementos e uma lista são átomos
(define
  lat?
  (lambda (x)
    (cond
      ((null? x) #t) ; Se chegar até o final da lista sem encontrar uma lista, então só pode ser que é composto de átomos.
      ((atom? (car x)) (lat? (cdr x)))
      (else #f)))) ; Se o elemento anterior no fluxo de controle não é um átomo, então a lista não é composta somente por átomos.

; Reimplementando member?

(define
  member?
  (lambda (a lat)
    (cond
      ((null? lat) #f) ; Se o último valor da lista é nulo, então a não é membro de lat.
      (else (or (eq? a (car lat)) (member? a (cdr lat))))))) ; Se a não é elemento de lat, então comparamos a ao restante da cauda. 
