#lang eopl

;; 7. cartesian-product:
;; Propósito:
;; L1, L2 -> L1 x L2: Procedimiento que toma dos listas
;; de símbolos no repetidos y retorna el producto cartesiano
;; de estas.
;; <lista> := ()
;;         := (<símbolo> <lista>)

;; pair-elements:
;; Propósito:
;; e, L2 -> e x L2: Procedimiento que toma un elemento e
;; y una lista L2, y retorna el producto cartesiano entre
;; dicho elemento y la lista.
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

;; appendAux:
;; Propósito:
;; L1, L2 -> L1 + L2: Procedimiento que toma dos listas y
;; retorna la concatenación de ambas.
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define appendAux
  (lambda (L1 L2)
    (if (null? L1) L2
      (cons (car L1) (appendAux (cdr L1) L2))
    )
  )
)

(define pair-elements
  (lambda (e L2)
    (if (null? L2) empty
      (cons (list e (car L2)) (pair-elements e (cdr L2)))
    )
  )
)

(define cartesian-product
  (lambda (L1 L2) 
    (if (null? L1) empty
      (appendAux (pair-elements (car L1) L2) (cartesian-product (cdr L1) L2))
    )
  )
)

;; Pruebas
(cartesian-product '(1 2 3 4) '(5 6 7 8))
(cartesian-product '(a b c d) '(e f g h))
(cartesian-product '(a) '(b c d))
(cartesian-product '(a b c) '(d))

;; 8. mapping:
;; Propósito:
;; F, L1 = (a1, a2, ..., an), L2 = (b1, b2, ..., bn) -> ((a1, b1), (a2, b2), ..., (an, bn)), donde
;; F(ai) = bi: Procedimiento que recibe una función unaria F, dos listas (ambas de igual tamaño), 
;; y retorna una lista de tuplas entre los elementos de ambas listas con la condición de que el 
;; primer elemento de cada tupla evaluado en F debe ser igual al segundo elemento de la tupla.
;; <lista> := ()
;;         := (<número> <lista>)

(define mapping
  (lambda (F L1 L2)
    (if (or (null? L1) (null? L2)) empty
      (if (= (F(car L1)) (car L2)) 
        (cons  (list (car L1) (car L2)) (mapping F (cdr L1) (cdr L2)))
        (mapping F (cdr L1) (cdr L2))
      )
    )
  )
)

;; Pruebas
(mapping (lambda (d) (* d 2)) '(1 2 3 4) '(2 4 6 8))
(mapping (lambda (d) (/ d 2)) '(1 2 3 4) '(2 4 6 8))
(mapping (lambda (d) d) '(1 2 3 4) '(1 2 3 4))

;; 9. inversions:
;; Propósito:
;; L -> n: Procedimiento que recibe una lista de valores numéricos y retorna un valor n que indica
;; cuantos pasos tomaria organizar la lista de menor a mayor (en el peor caso), es decir, retorna el
;; número de inversiones de la lista L1.
;; <lista> := ()
;;         := (<número> <lista>)

;; swapFirst:
;; Propósito:
;; L = (a1, a2, ..., an) -> L': Procedimiento que recibe una lista de valores numéricos y invierte el orden del primer par
;; de números que cumplan con la condición ai > aj si i < j.
;; <lista> := ()
;; <lista> := (<número> <lista>)

;; canSwamp?:
;; Propósito:
;; L -> True|False: Procedimiento que recibe una lista de valores numéricos y retorna True si hay una
;; posible inversión en la lista, False: de lo contrario.
;; <lista> := ()
;; <lista> := (<número> <lista>)

(define swapFirst
  (lambda (L)
    (if (or (null? L) (null? (cdr L))) L
      (if (> (car L) (cadr L)) 
        (cons (cadr L) (cons (car L) (cddr L))) 
        (cons (car L) (swapFirst (cdr L)))
      )
    )
  )
)

(define (canSwap? L)
  (cond
    [(or (null? L) (null? (cdr L))) #f]
    [(> (car L) (cadr L)) #t]
    [else (canSwap? (cdr L))]
  )
)

(define inversions
  (lambda (L)
    (if (canSwap? L) 
      (+ 1 (inversions(swapFirst L))) 
      0
    )
  )
)

;; Pruebas
(inversions '(1 2 3))
(inversions '(2 1 3 4))
(inversions '(4 3 2 1))
(inversions '(2 2 2 2))

;; 10. up:
;; Propósito:
;; L -> L': Procedimiento que recibe una lista y retorna la misma lista pero sin un nivel de
;; profundidad a cada elemento de la lista que sea una lista.
;; <lista> := ({<valor-de-scheme}*)

(define up
  (lambda (L)
    (if (null? L) 
      empty
      (appendAux (if (list? (car L)) (car L) (list (car L))) (up (cdr L)))
    )
  )
)

(up '(((x)) ((y)) (z)))
(up '(()))
(up '())

;; 13. operate:
;; Propóstio:
;; lrators = (a1, a2, ..., an), lrands = (b1, b2, ..., bn+1) -> v: Procedimiento que recibe una lista de funciones
;; binarias L1 de tamaño n, una lista de números L2 de tamaño n+1 y retorna el resultado de aplicar las
;; sucesivamente las funciones de L1 en L2.
;; <lista-func> := ({<operador>}+)
;; <operador> := + | - | * | / 
;; <lista-números> := ({<número-real>}+)

(define operate
  (lambda (lrators lrands)
    (if (null? lrators)
      (car lrands)
      (operate (cdr lrators) (cons ((car lrators) (car lrands) (cadr lrands)) (cddr lrands)))
    )
  )
)

;; Pruebas
(operate '() '(3))
(operate (list + +) '(1 1 1))
(operate (list + - * /) '(1 2 3 4 5))
(operate (list + - * /) '(2 3 4 5 6))

;; 18. pascal:
;; Propósito:
;; n -> L: Procedimiento que recibe un número natural n y retorna la n-ésima fila del triángulo
;; de pascal.
;; Para los cálculos se hará uso del 0 pero cabe decir que este no hace parte del dominio del
;; procedimiento.
;; <lista> := ({<natural>}+)
;; <natural> := 0 | 1 | 2 | ...

;; shiftLeft:
;; Propósito:
;; L -> L': Procedimiento que agrega un 0 al final de una lista.
;; <lista> := ({<natural>}+)
;; <natural> := 0 | 1 | 2 | ...

;; shiftRight:
;; Propósito:
;; L -> L': Procedimiento que agrega un 0 al inicio de una lista.
;; <lista> := ({<natural>}+)
;; <natural> := 0 | 1 | 2 | ...

;; shiftLeft:
;; Propósito:
;; L1 = (a1, a2, ..., an), L2 = (b1, b2, ..., bn) -> L1 + L2 = (a1+b1, a2+b2, ..., an+bn): Procedimiento 
;; que realiza una suma elemento a elemento entre dos listas, similar a una suma vectorial.
;; <lista> := ({<natural>}+)
;; <natural> := 0 | 1 | 2 | ...

(define shiftLeft
  (lambda (L)
    (if (null? L) 
      (list 0) 
      (cons (car L) (shiftLeft (cdr L)))
    )
  )
)

(define shiftRight
  (lambda (L)
    (if (null? L) 
      empty
      (cons 0 L)
    )
  )
)

(define addLists
  (lambda (L1 L2)
    (if (or (null? L1) (null? L2))
      empty
      (cons (+ (car L1) (car L2)) (addLists (cdr L1) (cdr L2)))
    )
  )
)

(define pascal
  (lambda (N)
    (if (= N 1) 
      (list 1) 
      (let ([prevPascal (pascal (- N 1))])
        (addLists (shiftLeft prevPascal) (shiftRight prevPascal))
      )
    )
  )
)

;; Pruebas
(pascal 6)
(pascal 7)
(pascal 8)
(pascal 9)
(pascal 10)