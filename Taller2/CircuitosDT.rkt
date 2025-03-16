#lang eopl

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

;; Pruebas

