#lang racket

(require graphviz file/convertible)

;; (scale (dot->pict
;;   (format
;;    "digraph {
;;       a [];
;;       b [label=\"c\"];
;;       c [label=\"\"];
;;       d [label=\"1/2\"];
;;       a -> d [label=\"{a}\"];
;;       a -> b [label=\"{a,b}\"];
;;       b -> c;
;;       d -> c;
;;       d -> g;
;;     }")) 1.5)


(struct lts (vertices edges) #:transparent)
(struct edge (in out label) #:transparent)


(define (draw-lts t)
  
  (define (draw-vertex x)
    (format "\"~a\";~n" x))

  (define (draw-vertices t)
    (string-append*
            (map (lambda (x) (draw-vertex x))
                 (lts-vertices t))))

  (define (draw-edge e)
    (format "\"~a\" -> \"~a\" [label=\"~a\"];~n"
            (edge-in e) (edge-out e) (edge-label e)))

  (define (draw-edges t)
    (string-append*
     (map (lambda (x) (draw-edge x)) (lts-edges t))))
  
  (scale (dot->pict
   (format "digraph {~n~a~a}"
           (draw-vertices t)
           (draw-edges t)))
         2))

(define (example)
  (draw-lts
   (lts (list "1/2s₁ + 1/2s₂"
              "s₂"
              "α")
        (list (edge "1/2s₁ + 1/2s₂" "s₂" "u")
              (edge "s₂" "α" "β")))))


(provide
 (struct-out edge)
 (struct-out lts)
 draw-lts)
