#lang racket

; Definições necessárias
(define ^
  (lambda (a1 a2)
    (cond
      ((zero? a2) 1)
      (else (* a1 (^ a1 (sub1 a2)))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Tentando criar um esqueleto para numbered (implementação já)
(define numbered?
  (lambda (x)
    (cond
      ((null? x) #t) ; Os elementos estão juntados a uma lista, então naturalmente retorno verdadeiro se o último dos elementos for uma lista.
      ((atom? x) (or (eq? x '+) (eq? x '*) (eq? x '^) (number?  x))) ; Se o elemento x for um átomo, vou identificar se o seu valor é um dos válidos (+, *, ^ ou se é número)
      (else (and (numbered? (car x)) (numbered? (cdr x)))))))
;
;
;; A primeira versão apresentada pelo autor é consideravelmente mais longa
;(define numbered?
;  (lambda (aexp)
;    (cond
;      ((atom? aexp) (number? aexp)) ; primeiro verifica se é número. Se for número, é válido.
;      ((eq? (car (cdr aexp)) '+) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))) ; se não é átomo é uma s-expression/lista. Nesse caso, no primeiro elemento do restante da lista é preciso identificar se há um dos elementos válidos.
;      ((eq? (car (cdr aexp)) '*) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))) ; Se o elemento válido é encontrado, então é preciso identificar se o primeiro elemento da expressão é válido, bem como o elemento após o símbolo encontrado também é válido.
;      ((eq? (car (cdr aexp)) '^) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

;; A simplificação seria:
;(define numbered?
;  (lambda (aexp)
;    (cond
;      ((atom? aexp) (number? aexp))
;      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

; Funciona porque não importa o símbolo entre os números, o importante é que entre a posição i e a posição i+2 os valores sejam números... Porém, é possível fazer com que esse tipo de expressão
; retorne verdadeiro e seja violada.

(numbered? '(3 + (4 ^ 5)))
(numbered? '(2 * sausage))
(numbered? '1)
(numbered? '((3 + 4) sausage (7 + 5))) ; Aqui é violada. A minha versão não retorna esse caso como verdadeiro!

; Minha versão de value. Falhou nos teste porque eu imaginei que poderia usar o valor do átomo simbólico para calcular. Não pareceu ser o caso


;(define value
;  (lambda (nexp)
;    (cond
;      ((numbered? nexp) ((car (cdr nexp)) (car nexp) (car (cdr (cdr nexp)))))
;      ((atom? nexp) nexp)
;      ((null? nexp) 0))))

;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      ((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
;      ((eq? (car (cdr nexp)) '*) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
;      (else (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))  

;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      ((eq? (car nexp) '+) (+ value (cdr nexp))(value (cdr (cdr nexp))))
;      ((eq? (car nexp) '*) (* (value (cdr nexp)) (value (cdr (cdr nexp)))))
;      (else (^ (value (car nexp)) (value (car (cdr (cdr nexp)))))))))  

; A função first 1st-sub-exp

;(define 1st-sub-exp
;  (lambda (nexp)
;    (car (cdr nexp))))
;
(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

;(define operator
;  (lambda (nexp)
;    (car nexp)))
;
;(1st-sub-exp '(+ 3 4))
;(2nd-sub-exp '(+ 3 4))
;(operator '(+ 3 4))


;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
;      ((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp))(value (2nd-sub-exp nexp))))
;      ((eq? (operator nexp) '*) (* (value (1st-sub-exp nexp))(value (2nd-sub-exp nexp))))
;      (else (*(value (1st-sub-exp nexp))(value (2nd-sub-exp nexp)))))))

;(value '(+ 3 4))

; Se for usar outro tipo de representação aritmética, é preciso modificar a função de primeira subexpressão e operador. Exemplo, o caso (3 + 4):
(define 1st-sub-exp
  (lambda (nexp)
    (car nexp)))

(define operator
  (lambda (nexp)
    (car (cdr nexp))))

(1st-sub-exp '(3 + 4))
(2nd-sub-exp '(3 + 4))
(operator '(3 + 4))

; Definindo sero?

(define sero?
  (lambda (x)
    (null? x)))

; Definindo edd1
(define edd1
  (lambda (x)
    (cons '() x)))

(edd1 '())
; Definindo zub1
(define zub1
  (lambda (x)
    (cdr x)))

(zub1 '(()))
(zub1 '(() ()))
(zub1 '(() () ()))

; Reescrevendo + nessa representação
(define +
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (+ n (zub1 m)))))))

(+ '(() ()) '(()))