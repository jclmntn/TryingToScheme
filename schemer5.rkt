#lang racket
;; Preciso de length!
;(define length
;  (lambda (lat)
;    (cond
;      ((null? lat) 0)
;      (else (add1 (length (cdr lat)))))))
;
;(length ' 1)
;
;; Acredito que precisarei de one também!

; Na verdade, eu precisaria de length pra garantir que o elemento da cabeça em questão
; não tem tamanho maior que um... Mas na verdade, a função que eu preciso mesmo é atom?

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Adicionando outras funções mencionadas
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; Redefinindo eqan?

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))


;Minha tentativa de rember*
; Lembrando que rember tira o primeiro a observado em lat
;(define rember*
;  (lambda (a lat)
;    (cond
;      ((null? lat) '())
;      (else (cond
;              ((not (atom? (car lat))) (cons (rember* a (car lat)) (rember* a (cdr lat)))) ; Se não é um átomo, então eu crio uma lista que junta o resultado de rember* aplicado ao topo e o resultado de rember* aplicado a cauda.
;              ((eq? a (car lat)) (rember* a (cdr lat))) ; Ao invés de entregar só a cauda, entrega o resultado da recursão da cauda, removendo todos os elementos.
;              (else (cons (car lat) (rember* a (cdr lat)))))))))

; Simplificando
(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat)) (cond ((eq? a (car lat)) (rember* a (cdr lat))) (else (cons (car lat) (rember* a (cdr lat)))))) ; Se é um átomo, vamos para um galho do fluxo de controle dedicado aos átomos.
      (else (cons (rember* a (car lat)) (rember* a (cdr lat))))))) ; Se não é um átomo, é uma lista. Então aplicamos rember* ao topo e à cauda da lista.


(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))


; Criando insertR*
(define insertR*
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat)) (cond ((eq? old (car lat)) (cons old (cons new (cdr lat)))) (else (cons (car lat) (insertR* new old (cdr lat))))))
      (else (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))

(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
;(insertR 'banana 'corinthians '(corinthians sao paulo flamengo))

; Criando occur*
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond ((eq? (car l) a) (add1 (occur* a (cdr l)))) (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))


; Criando subst*
(define subst*
  (lambda (new old l)
  (cond
    ((null? l) '())
    ((atom? (car l)) (cond ((eq? (car l) old) (cons new (subst* new old (cdr l)))) (else (cons (car l) (subst* new old (cdr l))))))
    (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'orange 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))

; Criando insertL
(define insertL*
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat)) (cond ((eq? old (car lat)) (cons new (cons old (insertL* new old (cdr lat))))) (else (cons (car lat) (insertL* new old (cdr lat))))))
      (else (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))))))

(insertL* 'pecker 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

; Criando member*
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l)))) ; Antes eu tinha criado uma versão que usava cond, mas é só simplificar usando or!
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(member* 'chips '((potato) (chips ((with) fish) (chips))))

; Criando leftmost
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost '(((hot) (tuna (and))) cheese))

; Criando eqlist: essa foi minha tentativa, bastante errada!
;(define eqlist?
;  (lambda (l1 l2)
;    (cond
;      ((and (null? l1) (null? l2)) #t)
;      ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;      (else (eqlist? (car l1) (car l2))))))
;


;(define eqlist?
;  (lambda (l1 l2)
;    (cond
;      ((and (null? l1) (null? l2)) #t)
;      ((or (null? l1) (null? l2)) #f) 
;      ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;      ((or (atom? (car l1)) (atom? (car l2))) #f)
;      (else
;       (and (eqlist? (car l1) (car l2))) (eqlist? (cdr l1) (cdr l2)))))) ; Algumas coisas eu peguei, mas outras errei feio.


; Definindo equal? Escrevi mais simplificado do que o autor!
(define equal?
  (lambda (arg1 arg2)
    (cond
      ((and (atom? arg1) (atom? arg1)) (eqan? arg1 arg2))
      ((or (atom? arg1) (atom? arg1)) #f)
      (else (eqlist? arg1 arg2)))))

; Rescrevendo eqlist usando equal?
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
(eqlist? '(beef ((salami)) (and (soda))) '(beef ((sausage)) (and (soda))))
(eqlist? '(strawberry ice cream) '(strawberry cream ice))

; Simplificando a nova rember

(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l)) 
      (else (cons (car l)) (rember s (cdr l))))))