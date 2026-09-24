(struct measure (elements) #:transparent)

(define (pair x y) (list x y))
(define R0 #e0)
(define R1 #e1)
(define R+ +)
(define R* *)
(define Rinv? zero?)
(define R/ /)


(define (dist-validity xs)
  (apply R+ (map second xs)))

(define/match (dist-map f xs)
  [(f  '())                  null]
  [(f  (cons (list x v) ys)) (cons (list (f x) v) (dist-map f ys))])

(define/match (dist-map-values f xs)
  [(f  '())                  null]
  [(f  (cons (list x v) ys)) (cons (list x (f v)) (dist-map-values f ys))])

(define/match (dist-remove-zeroes xs)
  [('())  null]
  [((cons (list x v) ys))
   (if (equal? v R0)
       (dist-remove-zeroes ys)
       (cons (list x v) (dist-remove-zeroes ys)))])

(define/match (dist-remove x xs)
  [(x '())  '()]
  [(x (cons (list x v) xs))  (dist-remove x xs)]
  [(x (cons (list y v) xs))  (cons (list y v) (dist-remove x xs))])

(define/match (dist-weight-of-point x xs)
  [(x '()) R0]
  [(x (cons (list x v) ys)) (R+ v (dist-weight-of-point x ys))]
  [(x (cons (list y v) ys)) (dist-weight-of-point x ys)])

(define/match (dist-reweight xs)
  [('()) '()]
  [((cons (list x v) ys))
   (let ([w   (R+ v (dist-weight-of-point x ys))])
     (cons (list x w) (dist-reweight (dist-remove x ys))))])

(define (dist-from-table l)
  (dist-map-values (lambda (v) (R/ v (dist-validity l))) l))

(define (dist-condense xs)
  (dist-reweight (dist-remove-zeroes xs)))

(define/match (dist-rescale xss)
  [((list xs v))  (dist-map-values (lambda (x) (R* v x)) xs)])

(define (dist-join xss)
  (dist-condense (apply append (map dist-rescale xss))))

(define (dist-normalize xs)
  (dist-condense (dist-map-values (lambda (v) (R/ v (dist-validity xs))) xs)))

(define (dist-bind xs f)
  (dist-join (dist-map f xs)))

(define (dist-return x)
  (list (pair x R1)))
