#lang racket

(define down(lambda (x)
(cond
[(empty? x) '()]
[else (cons (list(car x)) (down (cdr x)))]
)))




(define list-set(lambda (l n x)
(cond
[(zero? n) (cons x l)]
[else (cons (car l)(list-set (cdr l) (- n 1) x))]
)))


; EL APPEND NO ES MIO, USEN EL QUE CREARON .l.
(define aux(lambda (n x memory)
(let ( (node (car x))
  )
(cond
[(equal? n node) memory]
[(> n node) (aux n (caddr x) (append memory '("right")))]
[else (aux n (cadr x) (append memory '("left") ))]
))))


(define myTree '(8 (3 (1 ()()) (6(4()())(7()()))) (10 () (14(13()())()))))

(aux 13 myTree '())
