#lang racket

(require graphviz file/convertible)

(dot->pict
  (format
   "digraph {
      a [];
      b [label=\"c\"];
      c [label=\"\"];
      d [label=\"1/2\"];
      a -> d [label=\"{a}\"];
      a -> b [label=\"{a,b}\"];
      b -> c;
      d -> c;
      d -> g;
    }"))

