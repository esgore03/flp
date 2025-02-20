#lang racket

;(define down(lambda (x)
;(cond
;[(empty? x) '()]
;[else (cons (list(car x)) (down (cdr x)))]
;)))




(define list-set(lambda (l n x)
(cond
[(zero? n) (cons x l)]
[else (cons (car l)(list-set (cdr l) (- n 1) x))]
)))

(define x '(5 9 8))
(list-set x 2 "a")

