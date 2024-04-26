#lang r5rs

; Students: Rosny Miba, Paquet Timothé

; For the step 0, we based ourselves on the my-cons implementation.
; We defined a dispatcher function to handle a simple message m.
; Then this dispatcher is returned.
; The dispatcher returns only values.
; The variable containing a point is thus a dispatcher which can access to
; a closure where x and y are stored and available for this last while the link exist.

; point class
(define (point x y)
  (define (dispatch m)
    (cond ((eq? m 'getx) x)
          ((eq? m 'gety) y)
          ((eq? m 'type) 'point)
          ((eq? m 'info) (list 'point x y))
          ((eq? m 'info2) (list (dispatch 'type) (dispatch 'getx) (dispatch 'gety)))))
  dispatch)


; Yes, we can implement the 'info message in terms of
; a call to the other messages 'getx, 'gety and 'type
; look (p 'info2)



(define p (point 1 2))
(display (p 'getx)) ; 1
(display "\n")
(display (p 'gety)) ; 2
(display "\n")
(display (p 'type)) ; point
(display "\n")
(display (p 'info)) ; (point 1 2)