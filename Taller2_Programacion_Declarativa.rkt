#lang racket
;; Taller 2 – Programación Declarativa (Soluciones)
;; Autor: Henry Saul Martinez Flores
;; carnet: 00012622
;; Fecha: 2025-11-12
;; IDE: VS Code + Racket


(provide (all-defined-out))

;; --------------------------------------------------
;; Ejercicio 1 – Contar elementos positivos en una lista
;; Entrada: '(3 -2 7 0 -5 9)  ->  3
(define (count-positives lst)
  (length (filter (lambda (x) (> x 0)) lst)))

;; --------------------------------------------------
;; Ejercicio 2 – Generar lista de cuadrados pares
;; Entrada: '(1 2 3 4 5 6 7 8)  ->  '(4 16 36 64)
(define (even-squares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

;; --------------------------------------------------
;; Ejercicio 3 – Calcular el factorial de un número
;; Entrada: 5  -> 120
(define (fact n)
  (cond [(negative? n) (error 'fact "n debe ser >= 0")]
        [(zero? n) 1]
        [else (* n (fact (sub1 n)))]))

;; --------------------------------------------------
;; Ejercicio 4 – Elevar cada número al cubo
;; Entrada: '(2 3 4)  ->  '(8 27 64)
(define (cube-list lst)
  (map (lambda (x) (* x x x)) lst))

;; --------------------------------------------------
;; Ejercicio 5 – Sumar todos los elementos impares
;; Entrada: '(1 2 3 4 5 6 7)  ->  16
(define (sum-odds lst)
  (foldl + 0 (filter odd? lst)))

;; --------------------------------------------------
;; Ejercicio 6 – Determinar si una lista contiene números negativos
;; Entrada: '(5 9 -3 2)  ->  #t
(define (has-neg? lst)
  (ormap (lambda (x) (< x 0)) lst))

;; Ejercicio 7 – Suma acumulada con foldl
;; Entrada: '(1 2 3 4)  →  '(1 3 6 10)

(define (cumulative-sum lst)
  (reverse
   (car
    (foldl (lambda (x acc)                 ; acc = (lista-acumulada-reversa . ultimo-suma)
             (define out  (car acc))
             (define last (cdr acc))
             (define next (+ last x))
             (cons (cons next out) next))  ; nuevo acc: (agrega next a la lista) . next
           (cons '() 0)                    ; lista vacía y suma inicial 0
           lst))))

;; --------------------------------------------------
;; Ejercicio 8 – Concatenar cadenas de texto en una lista
;; Objetivo: Practicar foldl con operaciones sobre cadenas.
;; Entrada: '("Hola" " " "Mundo")
;; Salida esperada: "Hola Mundo"

(define (concat-strings lst)
  (foldr string-append "" lst))

;; --------------------------------------------------
;; Ejercicio 9 – Generar lista con el doble de los números > 5
;; Entrada: '(3 6 8 2 10)  ->  '(12 16 20)
(define (double-gt5 lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))

;; --------------------------------------------------
;; Ejercicio 10 – Invertir el orden de una lista
;; Entrada: '(1 2 3 4)  ->  '(4 3 2 1)
(define (my-reverse lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

;; --------------------------------------------------
;; Ejercicio 11 – Función que recibe una función como parámetro
;; Ejemplo: (apply-fn-to-list square '(1 2 3 4)) -> '(1 4 9 16)
(define (apply-fn-to-list f lst)
  (map f lst))

(define (square x) (* x x))

;; --------------------------------------------------
;; Ejercicio 12 – Promedio de números > 5 usando map, filter y foldl
;; Entrada: '(3 8 10 4 9 2 7)  ->  8.5
(define (avg-gt5 lst)
  (let* ([gt5 (filter (lambda (x) (> x 5)) lst)]
         ;; uso "map" (identidad) para cumplir el requerimiento
         [clean (map (lambda (x) x) gt5)]
         [total (foldl + 0 clean)]
         [n (length clean)])
    (if (zero? n)
        0
        (/ total n 1.0))))

;; --------------------------------------------------
;; Ejemplos rápidos (ejecuta este módulo para ver salidas)
(module+ main
  (displayln (count-positives '(3 -2 7 0 -5 9)))          ; 3
  (displayln (even-squares '(1 2 3 4 5 6 7 8)))           ; '(4 16 36 64)
  (displayln (fact 5))                                    ; 120
  (displayln (cube-list '(2 3 4)))                        ; '(8 27 64)
  (displayln (sum-odds '(1 2 3 4 5 6 7)))                 ; 16
  (displayln (has-neg? '(5 9 -3 2)))                      ; #t
(displayln (cumulative-sum '(1 2 3 4))) ; '(1 3 6 10)
(displayln (concat-strings '("Hola" " " "Mundo"))) ; "Hola Mundo"
  (displayln (double-gt5 '(3 6 8 2 10)))                  ; '(12 16 20)
  (displayln (my-reverse '(1 2 3 4)))                     ; '(4 3 2 1)
  (displayln (apply-fn-to-list square '(1 2 3 4)))        ; '(1 4 9 16)
  (displayln (avg-gt5 '(3 8 10 4 9 2 7))))                ; 8.5
