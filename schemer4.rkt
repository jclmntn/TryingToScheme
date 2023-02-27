#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;(atom? 14)

;(define n 14)
;(atom? 14)

;(atom? -3)

;(atom? 3.14159)

;(add1 67)

;(sub1 5)

;(sub1 0) ; A operação está definida nessa versão do Scheme.

;(zero? 0)

;(zero? 1492)

; Definindo a função +

;(define +
;  (lambda (arg1 arg2)
;  (cond
;    ((zero? arg1) arg2) ; Primeiro mandamento... Além disso, se o primeiro argumento é zero, só preciso retornar o segundo mesmo.
;    (else (+ (add1 arg1) (sub1 arg2)))))) ; Recursão!!!!!!!!!!! É como se estivéssemos tirando de arg2, de um em um, para adicionar a arg1. Mas está errado...
; Está errado porque a função fica se chamando de maneira infinita. Ela precisa terminar em algum momento!


;(define +
;  (lambda (arg1 arg2)
;  (cond
;    ((zero? arg2) arg1) ; Primeiro mandamento... Ajustando assim, a função funciona corretamente. Dos dois números o único que vira zero é o segundo argumento.
;    (else (+ (add1 arg1) (sub1 arg2)))))) ; Aqui ele soma 1 ao primeiro argumento n vezes e retorna arg1 quando arg2 é 0.

(define +
  (lambda (arg1 arg2)
    (cond
      ((zero? arg2) arg1)
      (else (add1 (+ arg1 (sub1 arg2))))))) ; Aqui ele vai guardando uns até chegar ao último elemento, que será arg1, definindo a soma 1 + 1 + 1 + ... + arg1. É interessante porque só um dos elementos é modificado.

;(- 18 25) ; A operação é definida no racket.

(define -
  (lambda (arg1 arg2)
    (cond
    ((zero? arg2) arg1)
    (else (sub1 (- arg1 (sub1 arg2))))))) ; Aqui ela pega dois argumentos e verifica se o segundo argumento é 0. Se for nulo, retornará o primeiro argumento. Se não for nulo, ele chama a função sub1 na recursão
; usando arg1 e arg2 subtraído de um. A ideia é que a função faz uma fila de menos uns (de tamanho arg2) e ao final, quando arg2 se torna 0, ele retorna arg1. Resultando em: arg1 - 1 - ... - 1.

(- 18 25) ; Funciona mesmo quando usando a nossa própria definição. Porém, não se pode usar números negativos.


; A função addtup não existe, vamos definí-la! Ela soma todos os números de uma tupla.

(define addtup
  (lambda (tup)
  (cond
    ((null? tup) 0)
    (else (+ (car tup) (addtup (cdr tup))))))) ; O equivalente a cons é +!
(addtup '(3 5 2 8))

; Definindo a função de multiplicação

(define *
  (lambda (arg1 arg2)
    (cond
      ((zero? arg2) 0) ; Nesse caso em específico 0, já que estou somando arg1 arg2 vezes.
      (else (+ arg1 (* arg1 (sub1 arg2))))))) ; Dessa vez, eu estou somando arg1 + ... + arg1 + 0.
(* 12 3)

; Definindo a função tup+
;(define tup+
;  (lambda (tup1 tup2)
;    (cond
;      ((and (null? tup1) (null? tup2)) '()) ; Tive dúvida do que retornar aqui, mas como estamos construindo listas, me parece plausível retornar uma lista vazia. ; Há um porém, essa condição só vale quando as duas listas estão vazias.
;      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))


;(define tup+
;  (lambda (tup1 tup2)
;    (cond
;      ((or (null? tup1) (null? tup2)) '()) ; Trocando por OR ainda não é bom o suficiente. Embora o resultado não seja perdido, seria mais interessante se pudessemos juntar o resto da segunda ou primeira lista ao que já foi calculado.
;      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;(define tup+
;  (lambda (tup1 tup2)
;    (cond
;      ((and (null? tup1) (null? tup2)) '())
;      ((null? tup1) tup2) ; Com essas duas condições adicionais eu consigo resolver o problema.
;      ((null? tup2) tup1)
;      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2) ; Não preciso avaliar se os dois estão vazios ao mesmo tempo. Se o primeiro está vazio, retorno o segundo e vice-versa.
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(2 3) '(4 6))
(tup+ '(3 7) '(4 6))
(tup+ '(4 6 8 1) '(3 7))

; Minha tentativa de definição de >

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f) ; Se n atinge zero antes de m, então m > n. (falso, pois a condição testa se n > m)
      ((zero? m) #t) ; Se m atinge zero antes de n, então n > m. (verdadeiro, pois a condição testa se n > m)
      (else (> (sub1 n) (sub1 m)))))) ; Acertei de primeira, porque levei em conta que a ordem das condições terminais são importantes.

(> 3 3)

; Minha tentativa de definição de <
; Fica bastante óbvia a solução!!
(define <
  (lambda (n m)
    (cond
      ((zero? m) #f) ; Se m atinge zero antes de n, então m < n. (falso, pois a condição testa se n < m)
      ((zero? n) #t) ; Se n atinge zero antes de m, então n < m. (verdadeiro, pois a condição testa se n < m)
      (else (< (sub1 n) (sub1 m)))))) 

(< 2 1)
; Reescrevendo = usando < e >

;(define =
;  (lambda (n m)
;    (cond
;    ((zero? m) (zero? n)) ; Se m atinge a condição de zero, retornar se n tirou a condição de zero. Se o resultado retornado for verdadeiro, então ambos são iguais!
;    ((zero? n) #f) ; Se n atingir zero antes de m, então não podem ser iguais. Perceba que existe uma simetria nessa função, podemos trocar m por n e n por m nas condições e o resultado será o mesmo!
;    (else (= (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(define ^
  (lambda (n m)
    (cond
    ((zero? m) 1)
    (else (* n (^ n (sub1 m)))))))

(^ 5 2)

(define /
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (/ (- n m) m))))))

; Minha tentativa para a função length

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0) ; Quando a lista está vazia, não preciso somar nada!
      (else (add1 (length (cdr lat))))))) ; Aplico length a cauda da lista, que não terá mais o primeiro elemento e somo 1 a esse resultado, que está contabilizando o primeiro elemento que ficará de fora na próxima recursão.

; Tentativa para a função pick
;(define pick
;  (lambda (n lat)
;    (cond
;    ((= n 1) (car lat))
;    ((> n (length lat)) '())
;    (else (pick (sub1 n) (cdr lat))))))
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
(pick 1 '(lasagna spaghetti ravioli macaroni meatball))


; Tentativa para a função rempick
;(define rempick
;  (lambda (n lat)
;    (cond
;      ((zero? (sub1 n)) (cdr lat)) ; Se chegarmos na posição em questão, devolvemos a cauda e ignoramos a ponta.
;      (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))) ; Como estamos construindo listas, usamos cons.


; Tentativa de no-nums
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat))) ; Realizo a recursão da cauda.
              (else (cons (car lat) (no-nums (cdr lat))))))))) ; Realizo a recursão do restante da lista, mas junto o primeiro elemento ao resultado.
(no-nums '(5 pears 6 prunes 9 dates))

; Tentativa de all-nums
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))
(all-nums '(5 pears 6 prunes 9 dates))

; Criando eqan?, que verifica se dois argumentos se tratam de um mesmo átomo
; Minha versão é menos enxuta que o livro, mas ela aceita a possibilidade de nulos... as condições são bem parecidas!!
; Não levo em conta, acidentalmente, a possibilidade de que dois átomos podem ser de tipos diferentes

;(define eqan?
;  (lambda (arg1 arg2)
;    (cond
;      ((and (null? arg1) (null? arg2)) #t)
;      (else (cond
;              ((and(number? arg1) (number? arg2)) (= arg1 arg2))
;              (else (eq? arg1 arg2)))))))

(define eqan?
  (lambda (arg1 arg2)
    (cond
      ((and (number? arg1) (number? arg2)) (= arg1 arg2))
      ((or (number? arg1) (number? arg2)) #f) ; Se não passou na primeira, mas passou na segunda, pelo menos um átomo é de tipo diferente do outro.
      (else (eq? arg1 arg2)))))

(eqan? 'r 'r)

; Definindo occur
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat)))))))) ; Usando eqan, pois o átomo pode ser número ou não!
(occur '1 '(1 b c d e 1 1 f))

; Definindo a função one?
;(define one?
;  (lambda (n)
;    (cond
;      ((zero? n) #f) ; N não pode ser zero, se for é falso
;      (else (zero? (sub1 n))))))  ; Se eu subtraio 1 de 1 será zero.

; Oneliner

(define one?
  (lambda (n)
    (eqan? 1 n)))

; Reescrevendo rempick

(define rempick
  (lambda (n lat)
    (cond
      ((null? lat) '())
      ((one? n) (cdr lat))    
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(rempick 3 '(hotdogs with hot mustard))
