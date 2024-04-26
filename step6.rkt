#lang r5rs

; Students: Rosny Miba, Paquet Timothé

; We modified a little the syntax of the step 5.
; We was not able to handle the info function but the
; rest seems to work

(define-syntax define-class
  (syntax-rules (define)
    ((_ (class class-args ...) super (super-body ...)
        (define (methods methods-args ...) methods-body ...) ...)
     
     (begin
       (define (class class-args ...)
         (define self 
           (lambda (method)
             (let ((method-find (assq method dispatcherlist)))
                 (if (eq? method-find #f)
                     (super method)
                     (cadr method-find))
             ))       
           )

         (define super (super-body ...))
         (define dispatcherlist
           (append `((methods ,(lambda (methods-args ...) methods-body ...)) ...
                   (set-self! ,(lambda (s) (set! self s) (send super 'set-self! s)))))
           )
         self)
       )

     )
    ))


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
(define-class (point x y) super (object)
  (define (getx) x)
  (define (gety) y)
  (define (type) 'point)
  ;(define (info) (list (send self 'type) (send self 'getx) (send self 'gety)))
  (define (add p) (point (+ x (send p 'getx)) (+ y (send p 'gety))))
)


; color-point class
(define-class (color-point x y color) super (point x y)
  (define (get-color) color)
  (define (type) 'color-point)
  ;(define (info) (append (send super 'info) (list color)))
  (define (add p) (new color-point (+ x (send p 'getx)) (+ y (send p 'gety)) color))
)


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



(define o (new object))
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
(define cp (new color-point 5 6 'red))
(display "\n")
(display (send cp 'type)) ; color-point
(display "\n")
(display (send cp 'getx)) ; 5
(display "\n")
(display (send cp 'gety)) ; 6
(display "\n")
(display (send cp 'get-color)) ; red
(display "\n")
(display (send cp 'info)) ; (color-point 5 6 red)
(define cp-1 (send cp 'add (color-point 1 2 'green)))
(display "\n")
(display (send cp-1 'type)) ; color-point
(display "\n")
(display (send cp-1 'getx)) ; 6
(display "\n")
(display (send cp-1 'gety)) ; 8
(display "\n")
(display (send cp-1 'get-color)) ; red