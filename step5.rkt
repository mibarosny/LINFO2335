#lang r5rs

; Students: Rosny Miba, Paquet Timothé

(define-syntax define-class
  (syntax-rules (define)
    ((_ (class class-args ...)
        (define self self-body ...)
        (define super super-body ...)
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
         (define super super-body ...)
         (define dispatcherlist
           (append `((methods ,(lambda (methods-args ...) methods-body ...)) ...
                   (set-self! ,(lambda (s) (set! self s) (send super 'set-self! s)))))
           )
         ; (display dispatcherlist)
         self)
       )

     )
    ))

(define (send receiver message . args)
  (if (procedure? receiver) ; check if the receiver object is indeed an appropriate receiver object 
      (apply (method-lookup receiver message) args)
      (display "Inappropriate receiver object")))

(define (method-lookup receiver message)
  (define (error . _) (display "Message not understood"))
  (if (eq? (receiver message) 'error)
      error
      (receiver message)))


(define (object)
  (define (type) 'object)
  (define (set-self! s) (set! self s))
  (define (self m)
    (cond ((eq? m 'type) type)
          ((eq? m 'set-self!) set-self!)
          (else 'error)))
  self)

(define-class (bank-account balance)
  ; init self and super
  (define self (object))
  (define super (object))
  
  ; ...

  (define (get-balance) balance)
  (define (withdraw n) (set! balance (- balance n)))
  (define (type_) 'bank-account)
)

(define (new class . class-args)
  (define self (apply class class-args))
  (send self 'set-self! self)
  self)

(define b (bank-account 56))

(define-class (point x y)
  (define self point)
  (define super (object))
  (define (getx) x)
  (define (gety) y)
  (define (type) 'point)
  (define (info) (list (send self 'type) (send self 'getx) (send self 'gety)))
  (define (add p) (point (+ x (send p 'getx)) (+ y (send p 'gety))))
)

(define-class (color-point x y color)
  (define self color-point)
  (define super (point x y))
  (define (get-color) color)
  (define (type) 'color-point)
  (define (info) (append (send super 'info) (list color)))
  (define (add p) (new color-point (+ x (send p 'getx)) (+ y (send p 'gety)) color))
)

