#lang r5rs

; Students: Rosny Miba, Paquet Timothé

; For the step 1, we define the function in the point closure.
; The dispatcher of this step returns now functions which can takes arguments.
; We need a first pair of parenthesis to obtain the desired function
; and another pair to us the function with her arguments.

; point class
(define (point x y)
  (define (getx) x)
  (define (gety) y)
  (define (type) 'point)
  (define (info) (list (type) (getx) (gety)))
  (define (add p) (point (+ x ((p 'getx))) (+ y ((p 'gety)))))
  (define (setx! a) (set! x a))
  (define (self m)
    (cond ((eq? m 'getx) getx)
          ((eq? m 'gety) gety)
          ((eq? m 'type) type)
          ((eq? m 'info) info)
          ((eq? m 'add) add)
          ((eq? m 'setx!) setx!)
          (else (lambda () (display "Message not understood")))))
  self)



(define p1 (point 1 2))
(define p2 (point 3 4))
(display ((p1 'getx))) ; returns 1
(display "\n")
(display ((p1 'gety))) ; returns 2
(display "\n")
(display ((p2 'getx))) ; returns 3
(display "\n")
(display ((p2 'gety))) ; returns 4
(define p ((p1 'add) p2)) ; returns a new point p
(display "\n")
(display ((p 'info))) ; returns (point 4 6)
(display "\n")
((p 'foo)) ; should display "Message not understood" error
((p1 'setx!) 5)
(display "\n")
(display ((p1 'getx))) ; returns 5