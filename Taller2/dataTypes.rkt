#lang eopl
;; Definición
(define-datatype circuit circuit?
  (a-circuit (gate-list gate-list?)))

(define-datatype gate-list gate-list?
  (empty-gate-list)
  (a-gate-list (first gate?)(rest gate-list?)))

(define-datatype gate gate?
  (a-gate (id symbol?) (type gate-type?) (input-list input-list?)))

(define-datatype gate-type gate-type?
  (and-type)
  (or-type)
  (not-type)
  (xor-type))

(define-datatype input-list input-list?
  (empty-input-list)
  (a-input-list (first input?) (rest input-list?)))

(define-datatype input input?
  (bool-input (value boolean?))
  (ref-input (gate-id symbol?)))

;; Constructores
(define (make-circuit gates)
  (a-circuit gates))

(define (make-gate id type inputs)
  (a-gate id type inputs))

;; Extractores
(define (circuit->gate-list c)
  (cases circuit c
    (a-circuit (gate-list) gate-list)))

(define (gate->id g)
  (cases gate g
    (a-gate (id type input-list) id)))

(define (gate->type g)
  (cases gate g
    (a-gate (id type input-list) type)))

(define (gate->input-list g)
  (cases gate g
    (a-gate (id type input-list) input-list)))

;; Pruebas
(define circuit1
  (make-circuit (a-gate-list 
    (make-gate 'G1 (not-type) (a-input-list (ref-input 'A) (empty-input-list)))
    (empty-gate-list))))

(define circuit2
  (make-circuit (a-gate-list 
    (make-gate 'G1 (and-type) (a-input-list (ref-input 'A) (a-input-list (ref-input 'B) (empty-input-list))))
    (empty-gate-list))))

(define circuit3
  (make-circuit (a-gate-list 
    (make-gate 'G1 (or-type) (a-input-list (ref-input 'A) (a-input-list (ref-input 'B) (empty-input-list))))
    (make-gate 'G2 (not-type) (a-input-list (ref-input 'G1) (empty-input-list)))
    (empty-gate-list))))

(define circuit4
  (make-circuit (a-gate-list 
    (make-gate 'G1 (or-type) (a-input-list (ref-input 'A) (a-input-list (ref-input 'B) (empty-input-list))))
    (make-gate 'G2 (and-type) (a-input-list (ref-input 'A) (a-input-list (ref-input 'B) (empty-input-list))))
    (make-gate 'G3 (not-type) (a-input-list (ref-input 'G2) (empty-input-list)))
    (make-gate 'G4 (and-type) (a-input-list (ref-input 'G1) (a-input-list (ref-input 'G3) (empty-input-list))))
    (empty-gate-list))))