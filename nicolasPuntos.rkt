;; 4. filter-in 
;; Propósito:
;; P, L = (a1, a2, ..., an) -> L' = (a1, a2, ..., an), tal que P(ai) = True: Procedimiento 
;; que toma un predicado y una lista y retorna una lista con los elementos que cumplen 
;; con el predicado.
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define filter-in
    (lambda (P L)
      (cond
        [(null? L) empty]
        [(P (car L)) (cons (car L) (filter-in P (cdr L))) ]
        [else (filter-in P (cdr L))]
        )
      )
    )

(filter-in list? '(1 2 (2 3) (5 6))) 
(filter-in string? '("carlos" "juan" "flp" a c 90 (7 8)))   
(filter-in number? '("albeiro" "univalle" a b c))

;; 6. swapper
;; Propósito:
;; E1, E2, L -> L': Procedimiento que toma dos elementos, una lista, y retorna una lista que
;; contiene el elemento E1 en la posición de todo elemento que antes era E2 y contiene el 
;; elemento E2 en la posición de todo elemento que antes era E1. 
;; <lista> := ()
;;         := (<símbolo> <lista>)


(define swapper
    (lambda (E1 E2 L)
      (cond
        [(null? L) empty]
        [(equal? E1 (car L)) (cons E2 (swapper E1 E2 (cdr L)))]
        [(equal? E2 (car L)) (cons E1 (swapper E1 E2 (cdr L)))]
        [else (cons (car L)(swapper E1 E2 (cdr L)))]
        )
      )
    )

(swapper 'a 'c '(h y c o a a))
(swapper 'b 'k '(b k b k b k))

;; 11. zip
;; Propósito:
;; F, L1 = (a1, a2, ..., an), L2 = (b1, b2, ..., bn) -> L' = (F(a1, b1), F(a2, b2), ..., F(an, bn)):
;; Procedimiento que toma una función binaria, dos listas, y retorna una lista donde cada elemento 
;; es el resultado de aplicar la función F sobre el par (ai, bi). 
;; <lista> := ()
;;         := (<número-real> <lista>)

(define zip
  (lambda (F L1 L2)
    (cond
      [(null? L1) empty]
      [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))]
      )
    )
  )

(zip - '(12 8 15) '(5 3 11))
(zip * '(4 3 1) '(5 6 2))


;; 12.filter-acum
;; Propósito:
;; a, b, F, acum, filter -> acum: Procedimiento que toma un valor a, un valor b, una función binaria
;; F, un acumulador acum y predicado filter, y retorna el resultado de aplicar F(a, acum) a todos los 
;; naturales entre a y b sii a cumple el predicado filter. 
;; <natural> := 0 | 1 | 2 | ...

(define filter-acum
  (lambda (a b F acum filter)
    (if (> a b) acum
        (cond
          [(filter a) (filter-acum (+ a 1) b F (F acum a) filter )]
          [else (filter-acum (+ a 1) b F acum filter)]
          )
        )
    )
  )

;; 15. count-odd-and-even

;; count-odd-all
;; Propósito:
;; arbol -> n: Procedimiento que toma un árbol y retorna la cantidad de números impares que hay
;; en el árbol.
;; <arbol-binario> ::= <int>
;;                 ::= (<int> <arbol-binario> <arbol-binario>) 

(define count-odd-all
  (lambda (arbol)
    (cond
      [(null? arbol) 0]
      [(number? arbol) (if (odd? arbol) 1
      0)]
      [else (+ (count-odd-all (car arbol)) (count-odd-all (cdr arbol)))]
      )
    )
  )

;; count-even-all
;; Propósito:
;; arbol -> n: Procedimiento que toma un árbol y retorna la cantidad de números pares que hay
;; en el árbol.
;; <arbol-binario> ::= <int>
;;                 ::= (<int> <arbol-binario> <arbol-binario>)

(define count-even-all
  (lambda (arbol)
    (cond
      [(null? arbol) 0]
      [(number? arbol) (if (even? arbol) 1
      0)]
      [else (+ (count-even-all (car arbol)) (count-even-all (cdr arbol)))]
      )
    )
  )

;; count-odd-and-even
;; Propósito:
;; arbol -> (a, b): Procedimiento que cuenta la cantidad de nodos del árbol que son pares y 
;; los que son impares.
;; <arbol-binario> ::= <int>
;;                 ::= (<int> <arbol-binario> <arbol-binario>)  

(define count-odd-and-even
  (lambda (arbol)
    (list (count-even-all arbol) (count-odd-all arbol))
    )
  )

(count-odd-and-even '(9 (7 () (5 () ()))
                        (12 (15 (8 () ())
                                ())
                            (16 () ()))))

(count-odd-and-even '(6 () ()))
(count-odd-and-even '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))

;; 16. Operar-binarias
;; recogSymbol
;; Propósito:
;; symbol -> F: Procedimiento que recibe un símbolo y retorna la función binaria correspondiente.
(define recogSymbol
  (lambda (symbol)
    (cond
      [(eq? symbol 'suma) +]
      [(eq? symbol 'resta) -]
      [else *]
      )
    )
  )

 ;; Operar-binarias
 ;; Propósito:
 ;; operacionB -> n: Procedimiento que recibe una operación binaria y retorna el resultado de
 ;; dicha operación.
 ;; <OperacionB>::= <int>
 ;;             ::= (<OperacionB> ’suma <OperacionB>)
 ;;             ::= (<OperacionB> ’resta <OperacionB>)
 ;;             ::= (<OperacionB> ’multiplica <OperacionB>) 

(define Operar-binarias
  (lambda (operacionB)
    (cond
      [(number? operacionB) operacionB]
      [else ((recogSymbol (cadr operacionB)) (Operar-binarias (car operacionB))
                                             (Operar-binarias (caddr operacionB)))]
      )
    )
  )

(Operar-binarias '(5 suma (4 multiplica 5)))
(Operar-binarias  '( (3 multiplica (2 suma 4) ) 31 multiplica ( (1 multiplica 2) resta 3 ) ) )

;; Punto 17
;; multParejas
;; Propósito:
;; L -> L': Procedimiento que recibe una lista de dos listas y retorna una lista que contiene 
;; el producto de los pares de las dos listas iniciales.
;; <lista> := ({<valor-de-scheme}*)

(define multParejas
  (lambda (P1 P2)
    (list (* (car P1) (car P2)) (* (cadr P1) (cadr P2)))
    )
  )

 ;; prod-scalar-matriz 
 ;; Propósito:
 ;; L -> L': Procedimiento que recibe una lista que tiene como elementos una matriz representada como
 ;; una lista de listas y un vector representado como una lista, y retorna el resultado de realizar 
 ;; la multiplicación matriz por vector como una lista. 
 ;; <lista> := ({<valor-de-scheme}*)

(define prod-scalar-matriz
  (lambda (mat vec)
    (if (null? mat) empty
        (cons (multParejas (car mat) vec) (prod-scalar-matriz (cdr mat) vec))
        )
    )
  )

(prod-scalar-matriz '((3 4) (1 5) (6 7)) '(2 2))
(prod-scalar-matriz '() '(2 2))