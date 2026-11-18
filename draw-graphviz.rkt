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
(struct vertex (name label) #:transparent)

(define (draw-lts t)
  
  (define (draw-vertex x)
    (format "~a [label=\"~a\"];"
            (vertex-name x)
            (vertex-label x)))

  (define (draw-vertices t)
    (string-append*
            (map (lambda (x) (draw-vertex x))
                 (lts-vertices t))))

  (define (draw-edge e)
    (format "~a -> ~a [label=\"~a\"];"
            (edge-in e) (edge-out e) (edge-label e)))

  (define (draw-edges t)
    (string-append*
     (map (lambda (x) (draw-edge x)) (lts-edges t))))
  
  (scale (dot->pict
   (format "digraph { ~a ~a }"
           (draw-vertices t)
           (draw-edges t)))
         2))

(define (example)
  (draw-lts
   (lts (list (vertex 's1 "1/2s₁ + 1/2s₂")
              (vertex 's2 "s₂")
              (vertex 'a  "α"))
        (list (edge 's1 's2 "u")
              (edge 'a 'c "β")))))


(provide
 (struct-out edge)
 (struct-out vertex)
 (struct-out lts)
 draw-lts)
