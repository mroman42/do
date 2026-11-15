#lang racket

(require rackunit)

;; Symmetric tricocycloids over Sets.
(struct tricocycloid (• ◦ ∗))

(define (i• p q) (* p q))
(define (i◦ p q) (/ (* p (- 1 q)) (- 1 (* p q))))
(define (i∗ p)   (- 1 p))
(define interval (tricocycloid i• i◦ i∗))


;; Testing the tricocycloid axioms.
(define (test trico p q r)
  (define (• p q) ((tricocycloid-• trico) p q))
  (define (◦ p q) ((tricocycloid-◦ trico) p q))
  (define (∗ p)   ((tricocycloid-∗ trico) p))
  (check-equal?  (• (• p q) r)              (• p (• q r)))
  (check-equal?  (• (◦ p (• q r)) (◦ q r))  (◦ (• p q) r))
  (check-equal?  (◦ (◦ p (• q r)) (◦ q r))  (◦ p q))
  (check-equal?  (∗ (∗ p))                  p)
  (check-equal?  (• (∗ (• p q)) (◦ p q))    (• p (∗ q)))
  (check-equal?  (◦ (∗ (• p q)) (◦ p q))    (∗ (◦ p (∗ q)))))

(define (test-comm trico p q r)
  (define (• p q) ((tricocycloid-• trico) p q))
  (define (◦ p q) ((tricocycloid-◦ trico) p q))
  (define (∗ p)   ((tricocycloid-∗ trico) p))
  (test trico p q r)
  (check-equal? (• p q) (• q p))
  (check-equal? (• (∗ (◦ p q)) (∗ q)) (• (∗ (◦ q p)) (∗ p)))
  (check-equal? (◦ (∗ (◦ p q)) (∗ q)) (∗ (◦ (∗ (◦ q p)) (∗ p))))
  )

;; Free tricocycloids
(struct f• (p q) #:transparent)
(struct f◦ (p q) #:transparent)
(struct f∗ (p)   #:transparent)

(define (fdraw p)
  (match p
    [(f• p q) (format "(~a • ~a)" (fdraw p) (fdraw q))]
    [(f◦ p q) (format "(~a ◦ ~a)" (fdraw p) (fdraw q))]
    [(f∗ p)   (format "∗~a" (fdraw p))]
    [x        (format "~a" x)]))

(struct ft (p)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc x port mode)
     (fprintf port "~a" (fdraw (ft-p x))))])

(define • (match-lambda** [((ft p) (ft q)) (ft (f• p q))]))
(define ◦ (match-lambda** [((ft p) (ft q)) (ft (f◦ p q))]))
(define ∗ (match-lambda** [((ft p))        (ft (f∗ p))]))
(define free-tricocycloid (tricocycloid • ◦ ∗))




;; Free tricocycloid algebras

(define (adraw p)
  (match p
    [(al x)     (format "~a" x)]
    [(a⊕ x p y) (format "(~a ⊕{~a} ~a)" (adraw x) p (adraw y))]))

(struct al (a)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc x port mode)
     (fprintf port "~a" (al-a x)))])

(struct a⊕ (x p y)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc x port mode)
     (fprintf port "(~a ⊕{~a} ~a)" (a⊕-x x) (a⊕-p x) (a⊕-y x)))])
