#lang eopl
;;Esteban Gomez Reyes 2330197
;;Jhorman Mauricio Gomez 2326867
;;Nicolas Salazar 2328060
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
      (list 'gate gate_id type (list 'empty_input_list))
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
      [(equal? (car gate_list) 'empty_gate_list) gate_list]
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
      [(equal? (car gate_list) 'empty_gate_list) gate_list]
      [(null? (cddr gate_list)) (list 'empty_gate_list)]
      [else (cons 'gate_list (cddr gate_list))]
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
      [(equal? (car input_list) 'empty_input_list) input_list]
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
      [(equal? (car input_list) 'empty_input_list) input_list]
      [(null? (cddr input_list)) (list 'empty_input_list)]
      [else (cons 'input_list (cddr input_list))]
    )
  )
) 

;; Pruebas

(circuit (gate_list (list (gate 'G1 (type 'not) (input_list '(A))))))
(circuit (gate_list (list (gate 'G1 (type 'and) (input_list '(A B))))))
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
(circuit->gate_list (circuit (gate_list (list (gate 'G1 (type 'not) (input_list '(A)))))))
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
            (list (gate 'G1 (type 'or) (input_list '())))
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
            (list (gate 'G1 (type 'or) (input_list '(A))))
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
        (list (gate 'G1 (type 'or) (input_list '(A))))
      )
    )
  )
)
(gate->input_list
  (gate_list->first
    (circuit->gate_list
      (circuit 
        (gate_list
          (list (gate 'G1 (type 'or) '())) 
        )
      )
    )
  )
)
