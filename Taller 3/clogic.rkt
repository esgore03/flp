#lang eopl

;*******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales y ligadura local

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expresion>
;;                  ::= let (identifier = expression)* in expression
;;  <primitive>     ::= + | - | * | add1 | sub1 
;;                  ::= eval-circuit | connect-circuits | merge-circuits
;;  <circuit>       ::= <gate_list>
;;  <gate_list>     ::= empty | <gate> <gate_list>
;;  <gate>          ::= <identifier> <type> <input list>
;;  <type>          ::= and | or | not | xor
;;  <input_list>    ::= empty | <bool> <input_list> | <identifier> <input_list>
;;  <bool>          ::= True | False
;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
  '(
    (white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
  )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    (program (expression) a-program)

    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression (primitive "("(separated-list expression ",")")") primapp-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression (circuit) circuit-exp)

    (circuit (gate-list) a-circuit)

    (gate-list ("") empty-gate-list)
    (gate-list (gate gate-list) a-gate-list)

    (gate (type identifier input-list) a-gate)

    (type ("and") and-type)
    (type ("or") or-type)
    (type ("xor") xor-type)
    (type ("not") not-type)

    (input-list ("") empty-input-list)
    (input-list (bool input-list) bool-input-list)
    (input-list (identifier input-list) var-input-list)

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") connect-circuits-prim)
    (primitive ("merge-circuits") merge-circuits-prim)

    (bool ("True") true-bool)
    (bool ("False") false-bool)
  )
)

;Construidos automáticamente:
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter))
)

;*******************************************************************************************
;Parser, Scanner, Interfaz

;FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter)
)

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter)
)

;El Interpretador (FrontEnd + Evaluación + señal para lectura )
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser scanner-spec-simple-interpreter grammar-simple-interpreter)
  )
)
;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body) (eval-expression body (init-env)))
    )
  )
)

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     '(4 2 5)
     (empty-env)
    )
  )
)

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
        (let 
          ((args (eval-rands rands env)))         
          (apply-primitive prim args)
        )
      )
      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)
        )
      )
      (let-exp (ids rands body)
        (let 
          ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))
        )
      )
    )
  )
)

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands))
)

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env))
)

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
    )
  )
)

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (cond
      [(equal? x "True") #t]
      [(equal? x "False") #f]
    )
  )
)

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?)) (vals (list-of scheme-value?)) (env environment?))
)

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record) ;llamado al constructor de ambiente vacío 
  )
)       

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)
  )
) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let 
          ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-env env sym)
          )
        )
      )
    )
  )
)

;*******************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)
  )
)

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else 
        (let 
          ((list-index-r (list-index pred (cdr ls))))
          (if (number? list-index-r)
            (+ list-index-r 1)
            #f
          )
        )
      )
    )
  )
)