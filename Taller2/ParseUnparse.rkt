#lang eopl

;; Circuitos Lógicos Basados en Listas
;; Propósito: Implementar la interfaz del TAD de circuitos lógicos basandose en listas.
;; <circuit> ::= '(circuit <gate_list>)
;; <gate_list> ::= empty | <gate> <gate_list>
;; <gate> ::= '(gate <gate id> <type> <input list>)
;; <gate_id> ::= identificador de la compuerta
;; <type> ::= and | or | not | xor
;; <input_list> ::= empty | <bool> <input_list> | <gate_ref> <input_list>
;; <gate ref> ::= identificador de otra compuerta

;; Constructores

;; circuit
;; Propósito: Procedimiento que recibe un tipo de dato gate_list y retorna un dato de tipo circuito.
;; <circuit> ::= '(circuit <gate_list>)
(define circuit
  (lambda (gate_list)
    (list 'circuit gate_list)
  )
)

;; gate_list
;; Propósito: Procedimiento que recibe una lista de datos de tipo gate y retorna un dato de tipo gate_list.
;; <gate_list> ::= empty | <gate> <gate_list>
(define gate_list
  (lambda (l)
    (cond
      [(null? l) (list 'empty_gate_list)]
      [else (cons 'gate_list l)]
    )
  )
)

;; gate
;; Propósito: Procedimiento que recibe un identificador, un dato de tipo type y un dato de tipo input_list
;; y retorna un dato de tipo gate.
;; un dato de tipo type.
;; <gate> ::= '(gate <gate id> <type> <input list>)
(define gate
  (lambda (gate_id type input_list)
    (if (null? input_list) 
      (list 'gate gate_id type '('empty_input_list))
      (list 'gate gate_id type input_list)
    )
  )
)

;; type
;; Propósito: Procedimiento que recibe un string correspondiente a un tipo de puerta lógica y retorna 
;; un dato de tipo type.
;; <type> ::= and | or | not | xor
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

;; input_list
;; Propósito: Procedimiento que recibe una lista de identificadores y booleanos y retorna un dato de tipo
;; input_list.
;; un dato de tipo type.
;; <input_list> ::= empty | <bool> <input_list> | <gate_ref> <input_list>
(define input_list
  (lambda (l)
    (cond 
      [(null? l) (list 'empty_input_list)]
      [else (cons 'input_list l)]
    )
  )
)

;; Extractores

;; circuit->gate_list
;; Propósito: A partir de un circuit retornar el gate_list asociado.
;; <circuit> ::= '(circuit <gate_list>)
(define circuit->gate_list
  (lambda (circuit)
    (cadr circuit)
  )
) 

;; gate_list->first
;; Propósito: A partir de un gate_list retornar el primer gate asociado.
;; <gate_list> ::= empty | <gate> <gate_list>
(define gate_list->first
  (lambda (gate_list)
    (cond 
      [(equal? (car gate_list) 'empty_gate_list) '('empty_gate_list)]
      [else (cadr gate_list)]
    )
  )
) 

;; gate_list->rest
;; Propósito: A partir de un gate_list retornar la lista de gates asociadas sin contar la cabeza.
;; <gate_list> ::= empty | <gate> <gate_list>
(define gate_list->rest
  (lambda (gate_list)
    (cond 
      [(equal? (car gate_list) 'empty_gate_list) '('empty_gate_list)]
      [(null? (cddr gate_list)) '('empty_gate_list)]
      [else (cddr gate_list)]
    )
  )
)

;; gate->gate_id
;; Propósito: A partir de un gate retornar el identificador asociado.
;; <gate> ::= '(gate <gate id> <type> <input list>)
(define gate->gate_id
  (lambda (gate)
    (cadr gate)
  )
) 

;; gate->type
;; Propósito: A partir de un gate retornar el tipo asociado.
;; <gate> ::= '(gate <gate id> <type> <input list>)
(define gate->type
  (lambda (gate)
    (caddr gate)
  )
) 

;; gate->input_list
;; Propósito: A partir de un gate retornar el input_list asociado.
;; <gate> ::= '(gate <gate id> <type> <input list>)
(define gate->input_list
  (lambda (gate)
    (cadddr gate)
  )
) 

;; input_list->first
;; Propósito: A partir de un input_list retornar el primer input asociado.
;; <input_list> ::= empty | <bool> <input_list> | <gate_ref> <input_list>
(define input_list->first
  (lambda (input_list)
    (cond 
      [(equal? (car input_list) 'empty_input_list) '('empty_input_list)]
      [else (cadr input_list)]
    )
  )
) 

;; input_list->rest
;; Propósito: A partir de un input_list retornar la lista de inputs asociados sin contar el primero
;; <input_list> ::= empty | <bool> <input_list> | <gate_ref> <input_list>
(define input_list->rest
  (lambda (input_list)
    (cond 
      [(equal? (car input_list) 'empty_input_list) '('empty_input_list)]
      [(null? (cddr input_list)) '('empty_input_list)]
      [else (cddr input_list)]
    )
  )
) 

;; Circuitos Lógicos Basados en Data-Types
;; Propósito: Implementar la interfaz del TAD de circuitos lógicos basandose en data-types
;; <circuit> ::= '(circuit <gate_list>)
;; <gate_list> ::= empty | <gate> <gate_list>
;; <gate> ::= '(gate <gate id> <type> <input list>)
;; <gate_id> ::= identificador de la compuerta
;; <type> ::= and | or | not | xor
;; <input_list> ::= empty | <bool> <input_list> | <gate_ref> <input_list>
;; <gate ref> ::= identificador de otra compuerta

(define-datatype circuit-exp circuit-exp?
  (a-circuit (gate-list gate-list-exp?)))

(define-datatype gate-list-exp gate-list-exp?
  (empty-gate-list)
  (a-gate-list (first gate-exp?) (rest gate-list-exp?)))

(define-datatype gate-exp gate-exp?
  (a-gate (id symbol?) (type type-exp?) (input-list input-list-exp?)))

(define-datatype type-exp type-exp?
  (and-type)
  (or-type)
  (not-type)
  (xor-type))

(define-datatype input-list-exp input-list-exp?
  (empty-input-list)
  (a-input-list (first input-exp?) (rest input-list-exp?)))

(define-datatype input-exp input-exp?
  (bool-input (value boolean?))
  (ref-input (id symbol?)))

;; PARSEBNF
;; Propósito: Procedimiento que recibe el llamado de un dato de tipo Circuito Lógico, una lista, luego divide
;; dicha lista en tokens y por último entrega como resultado la evaluación de la lista ingresada.
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
      [(boolean? exp) (bool-input exp)]
      [(symbol? exp) (ref-input exp)]
      [(equal? (car exp) 'circuit) (circuit (PARSEBNF (circuit->gate_list exp)))]
      [(equal? (car exp) 'empty_gate_list) (empty-gate-list)]
      [(equal? (car exp) 'gate_list)
        (a-gate-list (PARSEBNF(gate_list->first exp)) (myMap PARSEBNF (gate_list->rest exp)))
      ]
      [(equal? (car exp) 'gate) (a-gate (PARSEBNF (gate->gate_id exp)) (PARSEBNF (gate->type exp)) (PARSEBNF (gate->input_list exp)))]
      [(equal? (car exp) 'type)
        (cond
          [(equal? (cadr exp) 'or) (or-type)]
          [(equal? (cadr exp) 'and) (and-type)]
          [(equal? (cadr exp) 'xor) (xor-type)]
          [(equal? (cadr exp) 'not) (not-type)]
        )
      ]
      [(equal? (car exp) 'empty_input_list) (empty-input-list)]
      [(or (equal? (car exp) 'input_list) (or (symbol? (car exp)) (boolean? (car exp))))
        (a-input-list (PARSEBNF(input_list->first exp)) (myMap PARSEBNF (input_list->rest exp)))
      ]
      [else 'error]
    )
  )
)

;; Funciones Auxiliares

;; myMap
;; Propósito: Promedimiento que recibe una función unaria F, una lista L y retorna una lista con cada elemento
;; de la lista de entrada evaluado en F.
;; <lista> := (<función-unaria> <valor-de-scheme>)

(define myMap
  (lambda (F L)
    (if (null? L) 
      empty
      (cons (F (car L)) (myMap F (cdr L)))
    )
  )
)

;; myMap
;; Propósito: Promedimiento que recibe una función predicado unaria F, una lista L y retorna #t si todos
;; los elementos cumplen el predicado, #f en caso contrario.
;; <lista> := (<función-unaria> <valor-de-scheme>)

(define andMap
  (lambda (F L)
    (if (null? L) 
      #t
      (and (F (car L)) (andMap F (cdr L)))
    )
  )
)

(define circuit1 
  (circuit 
    (gate_list 
      (list 
        (gate 'G1 (type 'or) (input_list '(A B))) 
        (gate 'G2 (type 'not) (input_list '(G1)))
      )
    )
  )
)

(define gate_list1  
  (gate_list 
    (list 
      (gate 'G1 (type 'or) (input_list '(A B))) 
      (gate 'G2 (type 'not) (input_list '(G1)))
    )
  )
)

(define gate1.1 (gate 'G1 (type 'or) (input_list '(A B))))

(define gate1.2 (gate 'G2 (type 'not) (input_list '(G1))))

(define circuit2 (circuit (gate_list (list (gate 'G1 (type 'and) (input_list '(A B)))))))

(define gate_list2 (gate_list (list (gate 'G1 (type 'and) (input_list '(A B))))))

(define gate2 (gate 'G1 (type 'and) (input_list '(A B))))