#lang racket/base

(require racket/contract/base
         "private/container.rkt")

(provide
 (contract-out
  [read-container (-> input-port? list?)]
  [write-container (->* (string? list? output-port?)
                        (#:block-size exact-positive-integer?
                         #:compression (or/c 'none 'deflate))
                        void?)]))
