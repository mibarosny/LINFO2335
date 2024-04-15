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

(define o (object))
(send o 'type) ; object
;(send o 'foo) ; should display "Message not understood"
(define p1 (point 1 2))
(define p2 (point 3 4))
(send p1 'getx) ; 1
(send p1 'gety) ; 2
(send p2 'getx) ; 3
(send p2 'gety) ; 4
(define p (send p1 'add p2))
(send p 'info) ; (point 4 6)
(define cp (color-point 5 6 'red))
(send cp 'type) ; color-point
(send cp 'getx) ; 5
(send cp 'gety) ; 6
(send cp 'get-color) ; red
(send cp 'info)
; depending on your implementation this could result in
; (point 5 6 red) or (color-point 5 6 red)
; both of these are OK at this step, try to understand why
; more about this in step 4
(define cp-1 (send cp 'add (color-point 1 2 'green)))
(display (send cp-1 'type)) ; color-point
(display (send cp-1 'getx)) ; 6
(display (send cp-1 'gety)) ; 8
(display (send cp-1 'get-color)) ; red

