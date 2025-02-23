;Punto 4
(define filter-in
    (lambda (P L)
      (cond
        [(null? L) empty]
        [(P (car L)) (cons (car L) (filter-in P (cdr L))) ]
        [else (filter-in P (cdr L))]
        )
      )
    )

;Punto 6
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

;Punto 11
(define zip
  (lambda (F L1 L2)
    (cond
      [(null? L1) empty]
      [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))]
      )
    )
  )

;Punto 15
(define count-odd
  (lambda (num)
    (if (odd? num) 1
        0)))
(define count-even
  (lambda (num)
    (if (even? num) 1
        0)))
(define count-odd-all
  (lambda (arbol)
    (cond
      [(empty? arbol) 0]
      [(number? arbol) (count-odd arbol)]
      [else (+ (count-odd-all (car arbol)) (count-odd-all (cdr arbol)))]
      )
    )
  )
(define count-even-all
  (lambda (arbol)
    (cond
      [(empty? arbol) 0]
      [(number? arbol) (count-even arbol)]
      [else (+ (count-even-all (car arbol)) (count-even-all (cdr arbol)))]
      )
    )
  )

(define count-odd-and-even
  (lambda (arbol)
    (list (count-even-all arbol) (count-odd-all arbol))
    )
  )

;Punto 16
(define recogSymbol
  (lambda (symbol)
    (cond
      [(eq? symbol 'suma) +]
      [(eq? symbol 'resta) -]
      [else *]
      )
    )
  )

(define Operar-binarias
  (lambda (operacionB)
    (cond
      [(number? operacionB) operacionB]
      [else ((recogSymbol (cadr operacionB)) (Operar-binarias (car operacionB))
                                             (Operar-binarias (caddr operacionB)))]
      )
    )
  )
