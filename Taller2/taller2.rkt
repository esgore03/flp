#lang racket

;; Circuitos Lógicos
;; Propósito: Implementar la interfaz del TAD de circuitos lógicos basandose en listas.
;; <circuit> ::= '(circuit <gate_list>)
;; <gate_list> ::= empty | <gate> <gate_list>
;; <gate> ::= '(gate <gate id> <type> <input list>)
;; <gate_id> ::= identificador de la compuerta
;; <type> ::= and | or | not | xor
;; <input_list> ::= empty | <bool> <input_list> | <gate_ref> <input_list>
;; <gate ref> ::= identificador de otra compuerta


(define PARSEBNF
  (lambda (exp)
    (cond
      [(boolean? exp) exp]
      [(symbol? exp) exp]
      [(equal? (car exp) 'circuit) (circuit (PARSEBNF (circuit->gate_list exp)))]
      [(equal? (car exp) 'gate_list)
          (gate_list (myMap PARSEBNF (cdr exp)))
      ]
      [(equal? (car exp) 'gate) (gate (PARSEBNF (gate->gate_id exp)) (PARSEBNF (gate->type exp)) (PARSEBNF (gate->input_list exp)))]
      [(equal? (car exp) 'type) (type (PARSEBNF (cadr exp)))]
      [(equal? (car exp) 'input_list) 
          (input_list (myMap PARSEBNF (cdr exp)))
      ]
      [else 'error]
    )
  )
)

;; Constructores

(define circuit
  (lambda (gate_list)
    (list 'circuit gate_list)
  )
)

(define gate_list
  (lambda (l)
    (cond
      [(null? l) (list 'empty_gate_list)]
      [(list? (cadr l)) (cons 'gate_list l)]
      [else (list 'gate_list l)]
    )
  )
)

(define type
  (lambda (t)
    (cond 
      [(equal? t 'and) (list 'type t)]
      [(equal? t 'or) (list 'type t)]
      [(equal? t 'not) (list 'type t)]
      [(equal? t 'xor) (list 'type t)]
    )
  )
)

(define gate
  (lambda (gate_id type input_list)
    (if (null? input_list) 
      (list 'gate gate_id type '('empty_input_list))
      (list 'gate gate_id type input_list)
    )
  )
)

(define input_list
  (lambda (l)
    (cond 
      [(null? l) (list 'empty_input_list)]
      [else (cons 'input_list l)]
    )
  )
)

;; Extractores

(define circuit->gate_list
  (lambda (circuit)
    (cadr circuit)
  )
) 

(define gate_list->first
  (lambda (gate_list)
    (cond 
      [(equal? (car gate_list) 'empty_gate_list) '('empty_gate_list)]
      [else (cadr gate_list)]
    )
  )
) 

(define gate_list->rest
  (lambda (gate_list)
    (cond 
      [(equal? (car gate_list) 'empty_gate_list) '('empty_gate_list)]
      [(null? (cddr gate_list)) '('empty_gate_list)]
      [else (cddr gate_list)]
    )
  )
)

(define gate->gate_id
  (lambda (gate)
    (cadr gate)
  )
) 

(define gate->type
  (lambda (gate)
    (caddr gate)
  )
) 

(define gate->input_list
  (lambda (gate)
    (cadddr gate)
  )
) 

(define input_list->first
  (lambda (input_list)
    (cond 
      [(equal? (car input_list) 'empty_input_list) '('empty_input_list)]
      [else (cadr input_list)]
    )
  )
) 

(define input_list->rest
  (lambda (input_list)
    (cond 
      [(equal? (car input_list) 'empty_input_list) '('empty_input_list)]
      [(null? (cddr input_list)) '('empty_input_list)]
      [else (cddr input_list)]
    )
  )
) 

;; Funciones Auxiliares

(define myMap
  (lambda (F L)
    (if (null? L) 
      empty
      (cons (F (car L)) (myMap F (cdr L)))
    )
  )
)

(define andMap
  (lambda (F L)
    (if (null? L) 
      #t
      (and (F (car L)) (andMap F (cdr L)))
    )
  )
)

(define input_list?
  (lambda (input_list)
    (let ([first (input_list->first input_list)] [rest (input_list->rest input_list)])
      (cond 
        [(and (equal? (car input_list) 'empty_input_list) (null? (cdr input_list))) #t]
        [(and (equal? (car input_list) 'input_list) (not (null? rest))) (symbol? first)]
        [(equal? (car input_list) 'input_list) (and (symbol? first) (andMap symbol? rest))] 
        [else #f]
      )
    )
  )
)

(define gate?
  (lambda (gate)
    (let ([input_list (gate->input_list gate)])
      (and (equal? (car gate) 'gate) (input_list? input_list))
    )
  )
)

;; Pruebas

(circuit (gate_list (gate 'G1 (type 'not) (input_list '(A)))))
(circuit (gate_list (gate 'G1 (type 'and) (input_list '(A B)))))
(circuit 
  (gate_list 
    (list 
      (gate 'G1 (type 'or) (input_list '(A B))) 
      (gate 'G2 (type 'not) (input_list '(G1)))
    )
  )
)
(circuit 
  (gate_list
    (list
      (gate 'G1 (type 'or) (input_list '(A B))) 
      (gate 'G2 (type 'and) (input_list '(A B))) 
      (gate 'G3 (type 'not) (input_list '(G2))) 
      (gate 'G4 (type 'and) (input_list '(G1 G3)))
    )
  )
)
(circuit->gate_list (circuit (gate_list (gate 'G1 (type 'not) (input_list '(A))))))
(gate_list->first
  (circuit->gate_list
    (circuit 
      (gate_list
        (list
          (gate 'G1 (type 'or) (input_list '(A B))) 
          (gate 'G2 (type 'and) (input_list '(A B))) 
          (gate 'G3 (type 'not) (input_list '(G2))) 
          (gate 'G4 (type 'and) (input_list '(G1 G3)))
        )
      )
    )
  )
)
(gate_list->rest 
  (circuit->gate_list
    (circuit 
      (gate_list
        (list
          (gate 'G1 (type 'or) (input_list '(A B))) 
          (gate 'G2 (type 'and) (input_list '(A B))) 
          (gate 'G3 (type 'not) (input_list '(G2))) 
          (gate 'G4 (type 'and) (input_list '(G1 G3)))
        )
      )
    )
  )
)
(gate->gate_id
  (gate_list->first
    (circuit->gate_list
      (circuit 
        (gate_list
          (list
            (gate 'G1 (type 'or) (input_list '(A B))) 
            (gate 'G2 (type 'and) (input_list '(A B))) 
            (gate 'G3 (type 'not) (input_list '(G2))) 
            (gate 'G4 (type 'and) (input_list '(G1 G3)))
          )
        )
      )
    )
  )
)
(gate->type
  (gate_list->first
    (circuit->gate_list
      (circuit 
        (gate_list
          (list
            (gate 'G1 (type 'or) (input_list '(A B))) 
            (gate 'G2 (type 'and) (input_list '(A B))) 
            (gate 'G3 (type 'not) (input_list '(G2))) 
            (gate 'G4 (type 'and) (input_list '(G1 G3)))
          )
        )
      )
    )
  )
)
(gate->input_list
  (gate_list->first
    (circuit->gate_list
      (circuit 
        (gate_list
          (list
            (gate 'G1 (type 'or) (input_list '(A B))) 
            (gate 'G2 (type 'and) (input_list '(A B))) 
            (gate 'G3 (type 'not) (input_list '(G2))) 
            (gate 'G4 (type 'and) (input_list '(G1 G3)))
          )
        )
      )
    )
  )
)
(input_list->first
  (gate->input_list
    (gate_list->first
      (circuit->gate_list
        (circuit 
          (gate_list
            (list
              (gate 'G1 (type 'or) (input_list '(A B))) 
              (gate 'G2 (type 'and) (input_list '(A B))) 
              (gate 'G3 (type 'not) (input_list '(G2))) 
              (gate 'G4 (type 'and) (input_list '(G1 G3)))
            )
          )
        )
      )
    )
  )
)
(input_list->rest
  (gate->input_list
    (gate_list->first
      (circuit->gate_list
        (circuit 
          (gate_list
            (list
              (gate 'G1 (type 'or) (input_list '(A B))) 
              (gate 'G2 (type 'and) (input_list '(A B))) 
              (gate 'G3 (type 'not) (input_list '(G2))) 
              (gate 'G4 (type 'and) (input_list '(G1 G3)))
            )
          )
        )
      )
    )
  )
)
(input_list->first
  (gate->input_list
    (gate_list->first
      (circuit->gate_list
        (circuit 
          (gate_list
            (gate 'G1 (type 'or) (input_list '()))
          )
        )
      )
    )
  )
)
(input_list->rest
  (gate->input_list
    (gate_list->first
      (circuit->gate_list
        (circuit 
          (gate_list
            (gate 'G1 (type 'or) (input_list '(A)))
          )
        )
      )
    )
  )
)
(gate_list->first
  (circuit->gate_list
    (circuit 
      (gate_list
        '()
      )
    )
  )
)
(gate_list->rest
  (circuit->gate_list
    (circuit 
      (gate_list
        (gate 'G1 (type 'or) (input_list '(A)))
      )
    )
  )
)
(gate->input_list
  (gate_list->first
    (circuit->gate_list
      (circuit 
        (gate_list
          (gate 'G1 (type 'or) '()) 
        )
      )
    )
  )
)