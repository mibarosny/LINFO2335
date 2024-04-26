#lang r5rs

; Students: Rosny Miba, Paquet Timothé


; object class
(define (object)
  (define super 'nil)
  (define (type) 'object)
  (define (self m)
    (cond ((eq? m 'type) type)
          (else 'error)))
  self)


; point class
(define (point x y)
  (define super (object))
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
          (else (super m))))
  self)


; color-point class
(define (color-point x y color)
  (define super (point x y))
  (define (get-color) color)
  (define (type) 'color-point)
  (define (info) (append (send super 'info) (list (get-color))))
  (define (add p) (color-point (+ x ((p 'getx))) (+ y ((p 'gety))) color))
  (define (self m)
    (cond ((eq? m 'type) type)
          ((eq? m 'info) info)
          ((eq? m 'add) add)
          ((eq? m 'get-color) get-color)
          (else (super m))))
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



(define o (object))
(display (send o 'type)) ; object
(display "\n")
(send o 'foo) ; should display "Message not understood"
(define p1 (point 1 2))
(define p2 (point 3 4))
(display "\n")
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
(define cp (color-point 5 6 'red))
(display "\n")
(display (send cp 'type)) ; color-point
(display "\n")
(display (send cp 'getx)) ; 5
(display "\n")
(display (send cp 'gety)) ; 6
(display "\n")
(display (send cp 'get-color)) ; red
(display "\n")
(display (send cp 'info)) ; In our implementation : (point 5 6 red)
(define cp-1 (send cp 'add (color-point 1 2 'green)))
(display "\n")
(display (send cp-1 'type)) ; color-point
(display "\n")
(display (send cp-1 'getx)) ; 6
(display "\n")
(display (send cp-1 'gety)) ; 8
(display "\n")
(display (send cp-1 'get-color)) ; red