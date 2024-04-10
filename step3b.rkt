#lang r5rs

; Students: Rosny Miba, Paquet Timothé

(define (object)
  (define (type) 'object)
  (define (error . _) (display "Message not understood"))
  (define (self m)
    (cond ((eq? m 'type) type)
          (else error)))
  self)

(define (point x y)
  (define parent (object))
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
          (else (parent m))))
  self)



(define (color-point x y color)
  (define parent (point x y))
  (define (get-color) color)
  (define (type) 'color-point)
  (define (info) (append (send parent 'info) (list (get-color))))
  (define (add p) (color-point (+ x ((p 'getx))) (+ y ((p 'gety))) color))
  (define (self m)
    (cond ((eq? m 'type) type)
          ((eq? m 'info) info)
          ((eq? m 'add) add)
          ((eq? m 'get-color) get-color)
          (else (parent m))))
  self)

(define (send p . args)
  (if (procedure? p) ; check if the receiver object is indeed an appropriate receiver object 
      (let ((l (length args)))
        (cond ((= l 1) ((p (car args))))
              ((= l 2) ((p (car args)) (cadr args)))
              (else (display "Bad number of parameters"))))
      (display "Inappropriate receiver object")))



