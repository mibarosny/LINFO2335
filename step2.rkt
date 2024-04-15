#lang r5rs

; Students: Rosny Miba, Paquet Timothé

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

(define (send receiver message . args)
  (if (procedure? receiver) ; check if the receiver object is indeed an appropriate receiver object 
      (apply (method-lookup receiver message) args)
      (display "Inappropriate receiver object")))

(define (method-lookup receiver message)
  (define (error . _) (display "Message not understood"))
  (if (eq? (receiver message) 'error)
      error
      (receiver message)))
  