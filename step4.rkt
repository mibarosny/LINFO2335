#lang r5rs

; Students: Rosny Miba, Paquet Timothé


; object class
(define (object)
  (define super 'nil)
  (define (type) 'object)
  (define (set-self! s) (set! self s))
  (define (self m)
    (cond ((eq? m 'type) type)
          ((eq? m 'set-self!) set-self!)
          (else 'error)))
  self)


; point class
(define (point x y)
  (define super (object))
  (define (getx) x)
  (define (gety) y)
  (define (type) 'point)
  (define (info) (list (send self 'type) (send self 'getx) (send self 'gety)))
  (define (add p) (point (+ x (send p 'getx)) (+ y (send p 'gety))))
  (define (setx! a) (set! x a))
  (define (set-self! s) (set! self s) (send super 'set-self! s))
  (define (self m)
    (cond ((eq? m 'getx) getx)
          ((eq? m 'gety) gety)
          ((eq? m 'type) type)
          ((eq? m 'info) info)
          ((eq? m 'add) add)
          ((eq? m 'setx!) setx!)
          ((eq? m 'set-self!) set-self!)
          (else (super m))))
  self)


; color-point class
(define (color-point x y color)
  (define super (point x y))
  (define (get-color) color)
  (define (type) 'color-point)
  (define (info) (append (send super 'info) (list (get-color))))
  (define (add p) (new color-point (+ x (send p 'getx)) (+ y (send p 'gety)) color))
  (define (set-self! s) (set! self s) (send super 'set-self! s))
  (define (self m)
    (cond ((eq? m 'type) type)
          ((eq? m 'info) info)
          ((eq? m 'add) add)
          ((eq? m 'get-color) get-color)
          ((eq? m 'set-self!) set-self!)
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


; new method
(define (new class . class-args)
  (define self (apply class class-args))
  (send self 'set-self! self)
  self)



(define cp (new color-point 5 6 'red))
(display (send cp 'type)) ; color-point
(display "\n")
(display (send cp 'getx)) ; 5
(display "\n")
(display (send cp 'gety)) ; 6
(display "\n")
(display (send cp 'get-color)) ; red
(display "\n")
(display (send cp 'info)) ; (color-point 5 6 red)