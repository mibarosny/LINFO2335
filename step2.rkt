#lang r5rs

; Students: Rosny Miba, Paquet Timothé


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
          (else 'error))) 
  self)


; send method
(define (send receiver message . args)
  (if (procedure? receiver) ; check if the receiver object is indeed an appropriate receiver object 
      (apply (method-lookup receiver message) args)
      (display "Inappropriate receiver object")))


; method-lookup method
(define (method-lookup receiver message)
  (define (error . _) (display "Message not understood"))
  (if (eq? (receiver message) 'error)
      error
      (receiver message)))




(define p1 (point 1 2))
(define p2 (point 3 4))
(display (send p1 'getx)) ; 1
(display "\n")
(display (send p1 'gety)) ; 2
(display "\n")
(display (send p2 'getx)) ; 3
(display "\n")
(display (send p2 'gety)) ; 4
(define p (send p1 'add p2))
(display "\n")
(display (send p 'info)) ; (point 4 6)
(display "\n")
(send 'not-a-point 'info) ; should display "Inappropriate receiver object"
(display "\n")
(send p 'foo) ; should display "Message not understood"
(display "\n")
(send p 'bar 2) ; should display "Message not understood"
(send p1 'setx! 5)
(display "\n")
(display (send p1 'getx)) ; returns 5