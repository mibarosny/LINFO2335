#lang r5rs

; Students: Rosny Miba, Paquet Timothé

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
