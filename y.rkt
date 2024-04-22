#lang r5rs

(define-syntax define-class
  (syntax-rules (define)
    ((_ (class class-args ...) (define (methods methods-args ...) methods-body) ...)
     (begin
       (define (class class-args ...)
         (define dispatcherlist
           `((methods ,(lambda (methods-args ...) methods-body)) ...)
           )
         ;(display dispatcherlist)
         (lambda (method) (cadr (assq method dispatcherlist)))
         )
       )
     )
    ))
