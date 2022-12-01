#lang info

(define collection "avro")
(define deps '("base"
               "avro-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define implies '("avro-lib"))
(define scribblings '(("avro-manual.scrbl")))
