#lang racket

; Funções necessárias
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

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? a (car lat)) (member? a (cdr lat)))))))

(member? 'corinthians '(é muito legal o time corinthians))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember 'realmadrid '(a final do mundial de clubes terá o flamengo realmadrid flamengo))

(define firsts
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car (car lat)) (firsts (cdr lat)))))))
(firsts '((a b) (c d) (e f)))

; Minha defininção
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(set? '(a b c))
(set? '(apple 3 pear 4 9 apple 3 4))

; Minha tentativa de fazer a função makeset
; Igual a do livro! :)
;(define makeset
;  (lambda (lat)
;    (cond
;      ((null? lat) '())
;      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
;      (else (cons (car lat) (makeset (cdr lat)))))))

; Fazendo makeset com multirember...

;(define makeset
;  (lambda (lat)
;    (cond
;      ((null? lat) '())
;      ((member? (car lat) (cdr lat)) (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))
;      (else (cons (car lat) (makeset (cdr lat)))))))

; A versão do livro é mais simples
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(makeset '(apple peach pear peach plum apple lemon peach))

; Escrevendo a função subset: verifica se o primeiro argumento é um subconjunto do segundo argumento
;(define subset?
;  (lambda (set1 set2)
;    (cond
;      ((null? set1) #t)
;      ((member? (car set1) set2) (subset? (cdr set1) set2))
;      (else #f))))



; Usando (and ...) para a função subset
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(subset? '(5 chicken wings) '(5 hamburguers 2 pieces fried chicken and light ducking wings))
(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

; Para verificar se os dois conjuntos A e B são iguais, A precisa ser subconjunto de B e B precisa ser subconjunto de A.
; Vamos definir a função eqset!
;
;(define eqset?
;  (lambda (set1 set2)
;    (cond
;      ((and (subset? set1 set2) (subset? set2 set1)) #t)
;      (else #f))))

; Um jeito ainda mais fácil

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(eqset? '(a b c) '(c b a f))

; Definindo a função intersect?, para identificar se existe interseção entre dois conjuntos
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(intersect? '(a b c) '(d e))

; Definindo a função intersect, para identificar os elementos que dois conjuntos possuem em comum
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))

; Definindo a função union
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

; A diferença entre dois conjuntos é bem parecida com a união
(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

; Minha tentativa de implementação de intersectall
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(intersectall '((6 pears and) (3 peaches and 6 peppers) (8 pears and 6 plums) (and 6 prunes with some apples)))

; Um par é uma lista com dois elementos
; Essa é minha versão de a-pair
(define length
  (lambda (lat)
    (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat)))))))

(define a-pair?
  (lambda (lat)
    (equal? (length lat) 2)))

(a-pair? '(a b c))

; Redefinindo first, second and build

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))


(first '(a b))
(second '(a b))
(build 'a 'b)

; Third como uma oneliner
(define third
  (lambda (p)
    (car (cdr (cdr p)))))
(third '(a b c))

; Tentando definir a função fun?
(define fun?
  (lambda (rel)
      (set? (firsts rel))))

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fun? '((apples peaches) (pumpkin pie) (apples peaches)))

; Tentando definir a função revrel
;(define revrel
;  (lambda (rel)
;    (cond
;      ((null? rel) '())
;      (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

; Reescrevebdi revrel usando revpair
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick)))

; Definindo a função fullfun? (essa foi a minha versão... A versão do livro é um bocado diferente! Na verdade, parece mais com one-to-one!
; Na verdade, o meu erro é que minha função checa se uma relação é uma função injetora... A ideia dele é que estamos querendo entender
; se um objeto que já é função é uma função injetora! Lembrando que, ao menos nesse caso, trata-se de uma função se uma relação é um
; conjunto de pares.
;(define fullfun?
;  (lambda (rel)
;    (and (fun? rel) (fun? (revrel rel)))))

; Definindo seconds
(define seconds
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car (cdr (car lat))) (seconds (cdr lat)))))))

(seconds '((8 3) (4 2) (7 6) (6 2) (3 4)))

; Definindo fullfun como sugerido
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))
(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fullfun? '((grape raisin) (plum prune) (stewed grape)))

; Definindo one-to-one
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fullfun? '((grape raisin) (plum prune) (stewed grape)))