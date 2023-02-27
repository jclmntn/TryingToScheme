#lang racket
; Definindo funções necessárias
(define first
  (lambda (l)
    (car l)))
(define second
  (lambda (l)
    (first (cdr l))))

(first '(a b c))
(second '(a b c))


(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

; Definindo new-entry
(define new-entry build)

; Definindo lookup-in-entry
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) (entry-f))))))

; Definindo uma função extend-table
(define extend-table cons) ; Extender uma tabela com uma nova entrada é como adicionar uma expressão s a uma lista!

; Definindo lookup-in-entry
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (first table) (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

(lookup-in-table 'entrée '(((entrée dessert) (spaghetti spumoni)) ((appetizer entrée beverage) (food tastes good))) (lambda (name) 'empty))

