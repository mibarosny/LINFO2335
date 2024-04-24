#lang r5rs

; Students: Rosny Miba, Paquet Timothé

(define-syntax define-class
  (syntax-rules (define)
    ((_ (class class-args ...)
        (define (methods methods-args ...) methods-body ...) ...)
     (begin
       (define (class class-args ...)
         (define dispatcherlist
           `((methods ,(lambda (methods-args ...) methods-body ...)) ...)
           )
         
         (lambda (method)
           (cadr (assq method dispatcherlist)))
         )
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
  (define (super) (object))
  ; ...

  (define (get-balance) balance)
  (define (withdraw n) (set! balance (- balance n)))
  (define (type) 'bank-account)
)

(define b (bank-account 56))