#lang r5rs

(define (name-method method)
  (let* ((name (caadr method)))
    name
  ))
(define (lambda-method method)
  (let* ((proc method) (lambda-expr `(lambda ,(cdadr method) ,(caddr method))))
    lambda-expr
    ))
  
(define-syntax define-class
  (syntax-rules ()
    ((define-class (class args ... ) methods)
     (let ((met (name-method `methods)))
       (define a
         `(define (class args ...)
            (define methods-lambda (list ,(cdr met)))
                    
       ))
       
       (display a)
       )
     
     
     )))

(define-class (account balance)
  
  ; (display balance)
  ; (display 'moma)
  (define (get) balance)
  ;(define (get2) balance)
  ;(define (get3) balance)
  
  )

; (define a (account 4))
; ((a 'get))

(display "\n")(display "\n")(display "\n")

 