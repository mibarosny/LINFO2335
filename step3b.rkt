#lang r5rs

; Students: Rosny Miba, Paquet Timothé

(define (object)
  (define super 'nil)
  (define (type) 'object)
  (define (self m)
    (cond ((eq? m 'type) type)
          (else 'error)))
  self)

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


(define (send receiver message . args)
  (if (procedure? receiver) ; check if the receiver object is indeed an appropriate receiver object 
      (apply (method-lookup receiver message) args)
      (display "Inappropriate receiver object")))

(define (method-lookup receiver message)
  (define (error . _) (display "Message not understood"))
  (if (eq? (receiver message) 'error)
      error
      (receiver message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;


