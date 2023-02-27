#lang racket
; Definindo funções necessárias
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))


(define equal?
  (lambda (arg1 arg2)
    (cond
      ((and (atom? arg1) (atom? arg1)) (eqan? arg1 arg2))
      ((or (atom? arg1) (atom? arg1)) #f)
      (else (eqlist? arg1 arg2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define ^
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else (* a (^ a (sub1 b)))))))

(^ 2 6)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(length '(a b c))

; Definindo a função rember-f

;(define rember-f
;  (lambda (test? a l)
;    (cond
;      ((null? l) '())
;      ((test? a (car l)) (cdr l))
;      (else (cons (car l) (rember-f test? a (cdr l)))))))
;
;(rember-f = '5 '(6 2 5 3))
;(rember-f eq? '5 '(6 2 5 3))
;(rember-f equal? '5 '(6 2 5 3))


; Definindo a função eq-c
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))


; Definindo a função eq?-salad
(define eq?-salad
    (eq?-c 'salad))

(eq?-salad 'salad)
(eq?-salad 'tuna)

; Reescrevendo rember-f usando currying
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l) ((rember-f test?) a (cdr l)))))))) ; Perceba que é necessário chamar a função sendo definida no escopo local usando um argumento do escopo anterior (?)

((rember-f eq?) '5 '(6 2 5 3))

(define rember-eq?
  (rember-f eq?))

(rember-eq? 'tuna '(tuna salad is good))
((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))

; Definindo a função insertL
(define insertL-f
  (lambda (test?)
   (lambda (new old l)
     (cond
       ((null? l) '())
       ((test? (car l) old) (cons new l))
       (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

((insertL-f equal?) 'flamengo 'corinthians '(corinthians é o maior da terra))

; Definindo a função insertR

(define insertR-f
  (lambda (test?)
   (lambda (new old l)
     (cond
       ((null? l) '())
       ((test? (car l) old) (cons old (cons new (cdr l))))
       (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))
((insertR-f equal?) 'flamengo 'corinthians '(corinthians é o maior da terra))

; Definindo a função insert-g
; Faria isso usando funções auxiliares
; É exatamente o que o autor faz, embora o nome das funções seja diferente. 
(define right
  (lambda (new old l)
    (cons old (cons new l))))

(define left
  (lambda (new old l)
    (cons new (cons old l))))
;
;(define insert-g
;  (lambda (test?)
;    (lambda (type)
;      (lambda (new old l)
;        (cond
;        ((null? l) '())
;        ((test? (car l) old) (type new old l))
;        (else (cons (car l) (((insert-g test?) type) new old (cdr l)))))))))
;(((insert-g equal?) right) 'flamengo 'corinthians '(corinthians é o maior da terra))
;(((insert-g equal?) left) 'flamengo 'corinthians '(corinthians é o maior da terra))


; Vou substituir insert-g por uma versão mais simples.

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

((insert-g  right) 'flamengo 'corinthians '(corinthians é o maior da terra))
((insert-g  left) 'flamengo 'corinthians '(corinthians é o maior da terra))



; Agora vou definir uma função de substituição
((insert-g (lambda (new old l) (cons new (cdr l)))) 'flamengo 'corinthians '(corinthians é o maior da terra))

; Ou ainda, criando uma função auxilar
(define seqS
  (lambda (new old l) (cons new (cdr l))))

(define subst (insert-g seqS))
(subst 'flamengo 'corinthians '(corinthians é o maior da terra))

; teste yyy
(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(yyy 'sausage '(pizza with sausage and bacon)) ; Trata-se de um rember porque só retorna o resultado de cdr l no escopo anterior... não há cons com new e old!


; tentando defininr a função atom-to-function
(define 1st-sub-exp
  (lambda (nexp)
    (car (cdr nexp))))
(define 2nd-sub-exp
  (lambda (nexp)
    (car (cdr (cdr nexp)))))

(define operator
  (lambda (nexp)
    (car nexp)))

(define atom-to-function
  (lambda (x)
      (cond
        ((eq? x '+) +)
        ((eq? x '*) *)
        (else ^))))

(atom-to-function (operator '(+ 5 3)))

; Simplificando value!

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(value '(^ 5 3))

; Escrevendo multirember-f
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'flamengo '(flamengo fluminense flamengo flamengo corinthians))
((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))


; Definindo a função que testa se um valor é igual a tuna
;(define test?-eq
;  (lambda (value)
;    (eq? 'tuna value)))
;
;(test?-eq 'tuna)

;(define eq?-tuna
;  (eq?-c k))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))


; Definindo multiremember andco
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a) (multirember&co a (cdr lat) (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
      (else (multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)

; Definindo a função coletora
(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
         (cons 'tuna seen))))

(multirember&co 'tuna '(and tuna) a-friend)


; Final question
(define last-friend
  (lambda (x y)
    (length x)))

(multirember&co 'tuna '(strawberries tuna and swordfish) last-friend)


; Definindo MultiinsertLR
;(define multiinsertLR
;  (lambda (new old lat)
;    (cond
;      ((null? lat) '())
;      ((eq? (car lat) old) (cons new (cons old (cons new (multiinsertLR new old (cdr lat))))))
;      (else (cons (car lat) (multiinsertLR new old (cdr lat)))))))

; O método realizado pelo autor é um cadinho diferente
; Aqui a função meio que permite duas alternativas... Se uma palavra específica é encontrada, uma nova palavra pode ser inserida à esquerda; se outra palavra
; específica é encontrada, uma nova palavra é inserida à direita.
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(multiinsertLR 'corinthians 'flamengo 'flamengo '(flamengo))

; Definindo multiinsertLR&co (minha tentativa)
; Feliz em ver que me aproximei bastante do resultado do autor!
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0)) ; Três porque quando lat é nulo, col terá três elementos: a nova lista, as inserções à esquerda e as inserções à direita
      ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat left right) (col (cons new (cons oldL newlat)) (add1 left) right)))) ; Perceba que coleto os resultados sempre na lista newlat, independente da condição e incremento left e right conforme a necessidade.
      ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat left right) (col (cons oldR (cons new newlat)) left (add1 right)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat left right) (col (cons (car lat) newlat) left right)))))))

(multiinsertLR&co 'cranberries 'fish 'chips '() list) ; Vi esse truque de passar uma lista como col no stackoverflow e faz muito sentido... São três elementos resultantes da função coletora, então é só jogar esses três elementos pra uma lista!
(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) list)

; Definindo evens-only* Fiz um pequeno ajuste em even, já que nessa versão de lisp que estou usando
; a divisão retorna valores que não são inteiros
(define even?
  (lambda (n)
    (= (* (truncate (/ n 2)) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond ((even? (car l)) (cons (car l) (evens-only* (cdr l)))) (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

; Definindo evens-only&co
; Consegui fazer até o else final, depois disso fiquei com certa dúvida...
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l)) (cond
                         ((even? (car l)) (evens-only*&co (cdr l) (lambda (newlist prod sum) (col (cons (car l) newlist) (* (car l) prod) sum))))
                         (else (evens-only*&co (cdr l) (lambda (newlist prod sum) (col newlist prod (+ (car l) sum)))))))
      (else (evens-only*&co (car l) (lambda (alist aprod asum) (evens-only*&co (cdr l) (lambda (dlist dprod dsum) (col (cons alist dlist) (* aprod dprod) (+ asum dsum))))))))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) list) ; Entrega a nova lista de listas, o produto e a soma

(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
