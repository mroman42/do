#lang racket

(struct normReturn (vars) #:transparent)
(struct normObservation (var ovar next) #:transparent)
(struct normStatement (vars expr next) #:transparent)

(provide (struct-out normReturn))
(provide (struct-out normObservation))
(provide (struct-out normStatement))


(struct normProgram (program)
  #:transparent
  #:methods gen:custom-write
  ((define (show-statements indent p)
     (match p

       [(normReturn vars)
        (format "~areturn ~v" (make-string indent #\space) vars)]

       [(normObservation var ovar next)
        (format "~a'() <- (observe ~v ~v)\n~a"
                (make-string indent #\space)
                var ovar (show-statements indent next))]

       [(normStatement vars expr next)
        (format "~a~v <- ~a\n~a"
                (make-string indent #\space)
                vars (show-program (+ 4 indent) expr) (show-statements indent next))]))

   (define (show-program indent p)
     (define program (normProgram-program p))
     (if (or (normReturn? program) (normObservation? program) (normStatement? program))
           (string-append "(lDo Norm\n" (show-statements (+ 4 indent) program) ")")
           (format "~v" program)))

   (define (write-proc program port mode)
     (fprintf port (show-program 0 program)))))


(provide (struct-out normProgram))
; EXAMPLES
;; (normProgram
;;  (normStatement 'x (normProgram (normReturn 'x))
;;                 (normStatement 'y (normProgram (normStatement 'z (normProgram (normReturn 'x)) (normReturn 'z)))
;;                 (normObservation 'x 'y (normReturn '(y))))))
;(normProgram (normReturn '(y)))
;(normProgram 'p)

(define (memoize f)
  (let ([cache (make-hash)])
    (lambda (arg)
      (hash-ref! cache arg (lambda () (f arg))))))

(define temporary-name
  (memoize (lambda (x)
             (match (generate-temporaries (list x))
               [(list x) x]))))

(define (vars-to-syntax vs)
  (with-syntax ([(items ...) (cons #'list (map (lambda (v) (temporary-name v)) vs))])
    #'(items ...)))

(define (temporary-case vs)
  (if (list? vs)
      (vars-to-syntax vs)
      (temporary-name vs)))

(define (normReify s)
   (define (normReifyList s)
     (match s
       [(normProgram s)
          (if (or (normReturn? s)
                  (normObservation? s)
                  (normStatement? s))
           (append (list #'lDo #'Norm) (normReifyList s))
           (list s))]
       [(normReturn vs)
        (if (list? vs)
            (list #'return (vars-to-syntax vs))
            (list #'return (temporary-name vs)))]
       [(normObservation x y next)
        (append (list #'(list) #'<- #`(observe #,(temporary-name x) #,(temporary-name y)))
                (normReifyList next))]
       [(normStatement x e next)
        (append (list (temporary-case x) #'<- (normReify e))
                (normReifyList next))]
       [p (list p)]))
  (with-syntax ([(items ...) (normReifyList s)])
     #'(items ...)))


(provide normReify)

;; Usage:

;(slow-identity "data") ; Takes 1 second
;(slow-identity "data") ; Returns instantly
