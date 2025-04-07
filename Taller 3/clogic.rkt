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
    (expression ("True") true-bool-exp)
    (expression ("False") false-bool-exp)

    (circuit ("circuit" "("gate-list")") a-circuit)

    (gate-list ("empty-gate-list") empty-gate-list)
    (gate-list ("gate-list" "("gate (arbno gate)")") a-gate-list)

    (gate ("gate" "("identifier type input-list")") a-gate)

    (type ("and") and-type)
    (type ("or") or-type)
    (type ("xor") xor-type)
    (type ("not") not-type)

    (input-list ("empty-input-list") empty-input-list)
    (input-list ("input-list" "(" expression (arbno expression)")") an-input-list)

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("eval-circuit") eval-circuit-prim)
    (primitive ("connect-circuits") connect-circuits-prim)
    (primitive ("merge-circuits") merge-circuits-prim)
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
     '(c1)
     (list (a-circuit (empty-gate-list)))
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
          (apply-primitive prim args env)
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
      (circuit-exp (circuit) circuit)
      (true-bool-exp () #t)
      (false-bool-exp () #f)
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
  (lambda (prim args env)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (eval-circuit-prim ()
        (let ([circ (car args)])
          (eval-gate-list (circuit->gate-list circ) env)
        )
      )
      (connect-circuits-prim ()
        (let ([c1 (car args)] [idToReplace (caddr args)])
          (replaceIdentifier (returnLastIdentifier c1) idToReplace)
        )
      )
      (else 'meFalta)
    )
  )
)

(define circuit->gate-list
  (lambda (circ)
    (cases circuit circ
      (a-circuit (gate-list) gate-list)
    )
  )
)

(define gate-list->first
  (lambda (glist)
    (cases gate-list glist
      (empty-gate-list () empty)
      (a-gate-list (first rest) first)
    )
  )
)

(define gate-list->rest
  (lambda (glist)
    (cases gate-list glist
      (empty-gate-list () empty)
      (a-gate-list (first rest) rest)
    )
  )
)

(define gate->identifier
  (lambda (g)
    (cases gate g
      (a-gate (id type input-list) id)
    )
  )
)

(define returnLastIdentifier
  (lambda (circ)
    (let* ([glist (circuit->gate-list circ)] [first (gate-list->first glist)] [rest (gate-list->rest glist)])
      (if (null? first)
        (eopl:error 'returnLastIdentifier "No last identifier for an empty circuit")
        (returnLastIdentifierAux first rest)
      )
    )
  )
)

(define returnLastIdentifierAux
  (lambda (first rest)
    (if (null? rest)
      (gate->identifier first)
      (returnLastIdentifierAux (car rest) (cdr rest))
    )
  )
)

(define replaceIdentifier 
  (lambda (newId oldId)
    empty
  )
)

(define compareAndReplaceIdentifiers
  (lambda (g id)
    empty
  )
)

;; función que procesa un gate-list aplicandole cases y obtieniendo los elementos de este para luego
;; invocar a la función auxiliar auxRecursiveEvalGate.
(define eval-gate-list
  (lambda (glist env)
    (let ([first (gate-list->first glist)] [rest (gate-list->rest glist)])
      (if (null? first)
        (eopl:error 'eval-gate-list "No evaluation for gate-list with no gates")
        (auxRecursiveEvalGate first rest env)
      )
    )
  )
)

;; función auxiliar recursiva utilizada para la evaluación de un gate-list.
(define auxRecursiveEvalGate
  (lambda (first rest env)
    (let* ([res (eval-gate first env)] [newEnv (extend-env (list (car res)) (list (cadr res)) env)])
      (if (null? rest) 
        (cadr res)
        (auxRecursiveEvalGate (car rest) (cdr rest) newEnv)
      )
    )    
  )
)

;; función que permite evaluar un gate de acuerdo a su tipo. Retorna una lista donde el primer elemento
;; es el id del gate que se evaluó y el segundo elemento es el valor de verdad.
(define eval-gate
  (lambda (g env)
    (cases gate g
      (a-gate (id t inputList) 
        (cases input-list inputList
          (an-input-list (first rest) 
            (cases type t
              (and-type () (list id (and (eval-input-item first env) (eval-input-item (car rest) env))))
              (or-type () (list id (or (eval-input-item first env) (eval-input-item (car rest) env))))
              (not-type () (list id (not (eval-input-item first env))))
              (xor-type () (list id (xor (eval-input-item first env) (eval-input-item (car rest) env))))
            )
          )
          (empty-input-list () (eopl:error 'eval-gate "No evaluation for gate with no inputs"))
        )
      )
    )
  )
)

;; función que permite procesar los inputs de un input-list. Esta función es necesaria porque se requiere
;; tener un control de que tipo de expressions pueden ser procesadas en un input-list puesto que por
;; definición un input-list recibe cualquier tipo de expression.
(define eval-input-item
  (lambda (input env)
    (cases expression input
      (true-bool-exp () #t)
      (false-bool-exp () #f)
      (var-exp (id) (apply-env env id))
      (else (eopl:error 'eval-input-item "Unsupported type for input-item"))
    )
  )
)

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))
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

; función utilizada para implementar xor en nuestro lenguaje.
(define xor 
  (lambda (a b)
    (and (or a b) (not (and a b)))
  )
)

(interpretador)

;; Ejemplos eval-circuit

"let C1 = circuit(empty-gate-list) in eval-circuit(C1)"

"let A = True C1 = circuit(gate-list(gate(G1 not input-list(A)))) in eval-circuit(C1)"

"let A = True B = True C1 = circuit(gate-list(gate(G1 and input-list(A B)))) in eval-circuit(C1)"

"let A = True B = True C1 = circuit(gate-list(gate(G1 or input-list(A B)) gate(G2 and input-list(A B)) gate(G3 not input-list(G2)) gate(G4 and input-list(G1 G3)))) in eval-circuit(C1)"
"let A = False B = True C1 = circuit(gate-list(gate(G1 or input-list(A B)) gate(G2 and input-list(A B)) gate(G3 not input-list(G2)) gate(G4 and input-list(G1 G3)))) in eval-circuit(C1)"
"let A = False B = False C1 = circuit(gate-list(gate(G1 or input-list(A B)) gate(G2 and input-list(A B)) gate(G3 not input-list(G2)) gate(G4 and input-list(G1 G3)))) in eval-circuit(C1)"
"let A = True B = False C1 = circuit(gate-list(gate(G1 or input-list(A B)) gate(G2 and input-list(A B)) gate(G3 not input-list(G2)) gate(G4 and input-list(G1 G3)))) in eval-circuit(C1)"

;; Ejemplos connect-circuits

;; Ejemplos merge-circuits
