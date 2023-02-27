#lang racket

;q1
; Rember não existe! Preciso definí-la

;(define rember
;  (lambda (a lat)
;  (cond
;    ((null? lat) '())
;    (else (cond
;            ((eq? a (car lat)) (cdr lat)) ; Do jeito que está, essa função só retorna o valor esperado quando o primeiro elemento da lista é a.
;            (else (rember a (cdr lat))))))))

; A versão correta de rember usa cons a seu favor
; (define rember
;  (lambda (a lat)
;  (cond
;    ((null? lat) '())
;    (else (cond
;            ((eq? a (car lat)) (cdr lat))
;            (else (cons (car lat) (rember a (cdr lat)))))))))



; Reescrevendo
;(define rember
;  (lambda (a lat)
;    (cond
;      ((null? lat) '())
;      (else(cond
;             ((eq? a (car lat)) (cdr lat))
;             (else (cons (car lat) (rember a (cdr lat)))))))))

;(define lat '(bacon lettuce and tomato)) 
;(define a 'and)
;(rember a lat)

; Simplificando
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define lat '(soy sauce and tomato sauce)) 
(define a 'sauce)
(rember a lat) ; Esperamos '(soy and tomato sauce)

;; firsts: não definida
; Em teoria, ela deve pegar o primeiro elemento de uma lista de listas.

(define firsts
  (lambda (lat)
    (cond
      ((null? lat) '()) ; Checa se a lista é nula
      (else (cons (car (car lat)) (firsts (cdr lat))))))) ; Junta o primeiro elemento da primeira lista com a recursão da cauda da lista.
      

(define l '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant))) ;O resultado esperado é o '(apple plum grape bean)
(firsts l)

;;insertR não definida
; Possui três argumentos: a nova palavra, a palavra que ela tomará a posição (à direita) e a lista de palavras.
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons old (cons new (cdr lat)))) ; Se o primeiro átomo da lista é a palavra antiga, juntamos a palavra antiga e a nova com a cauda após a palavra antiga
              (else (cons (car lat) (insertR new old (cdr lat))))))))) 
;; Por outro lado, se o elemento não foi encontrado, resta juntar o primerio elemento da lista com a recursão de insertR (da cauda), pois, não queremos que elementos iniciais sejam perdidos.
;; Exemplo
;
(define new 'jalapeño)
(define old 'and)
(define lat2 '(tacos tamales and salsa))
;(insertR new old lat2)

;; Com insertR definido, temos de definir insertL
; É basicamente insertR, mas mudamos a ordem no cons.
;(define insertL
;  (lambda (new old lat)
;    (cond
;      ((null? lat) '())
;      (else (cond
;              ((eq? (car lat) old) (cons new (cons old (cdr lat)))) 
;              (else (cons (car lat) (insertL new old (cdr lat)))))))))



; Perceba que (cons old (cdr lat)) é a mesma coisa que lat, então...
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new lat)) 
              (else (cons (car lat) (insertL new old (cdr lat)))))))))
(insertL new old lat2)

; Agora definimos substr, que pode se aproveitar da estrutura anterior;
; Na primeira condição após a verificação de nulidade, podemos fazer (cons new (cdr lat)), já que (eq? (car lat) old).
; Estaríamos juntando new a cauda de lat, o que remove o início de lat.
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new (cdr lat))) 
              (else (cons (car lat) (subst new old (cdr lat)))))))))

(define lat3 '(ice cream with fudge for dessert))
(define new2 'topping)
(define old2 'fudge)
(subst new2 old2 lat3)

; subst2 substituí o1 ou o2 (o que ocorrer primeiro) por new

;(define subst2
;  (lambda (new o1 o2 lat)
;    (cond
;      ((null? lat) '())
;      (else (cond
;              ((eq? (car lat) o1) (cons new (cdr lat)))
;              ((eq? (car lat) o2) (cons new (cdr lat)))
;              (else (cons (car lat) (subst2 new old (cdr lat)))))))))

(define lat4 '(banana ice cream with chocolate topping))
(define new3 'vanilla)
(define o1 'chocolate)
(define o2 'banana)

; Podemos juntar as duas linhas de cond em uma única linha usando or!
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
              (else (cons (car lat) (subst2 new old (cdr lat)))))))))

(subst2 new3 o1 o2 lat4)

; Vamos definir a função multirember agora, que remove todos a de um lat
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '()) 
      (else (cond
              ((eq? a (car lat)) (multirember a (cdr lat))) ; Se o primeiro elemento é a, então não preciso dele e faço a recursão da cauda
              (else (cons (car lat) (multirember a (cdr lat))))))))) ; Caso contrário (o primeiro elemento não é a), então guardo primeiro elemento com a recursão da cauda.

(multirember 'cup '(coffee cup tea cup and hick cup))

; Vamos definir agora a função multinsertR, que adiciona a direita uma nova palavra a cada ocorrência de uma determinada palavra

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
       ; O tema geral aqui é que: ainda preciso adicionar o velho e o novo à cauda, mas a cauda precisa passar por um tratamento!
      (else (cond
              ((eq? (car lat) old) (cons old(cons new (multiinsertR new old (cdr lat))))) ; Por que o Schemer fica pedindo pra criar condições dentro do else?
              (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

(multiinsertR 'banana 'corinthians '(o são paulo corinthians perdeu para o corinthians))

; Agora podemos definir a multiinsertL
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
              (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

(multiinsertL 'banana 'corinthians '(o são paulo corinthians perdeu para o corinthians))
(multiinsertL 'fried 'fish '(chips and fish or fish and fried))

; Definindo a função de substituição múltipla
(define multisubst
  (lambda (new old lat)
  (cond
    ((null? lat) '())
    (else (cond
            ((eq? (car lat) old) (cons new (multisubst new old (cdr lat)))) ; Pegamos o topo modificado (new) e juntamos a cauda modificada (passou por recursão).
            (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(multisubst 'a 'b '(a b a b))