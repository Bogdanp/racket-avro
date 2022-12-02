#lang racket/base

(require racket/contract
         "private/codec.rkt")

(provide
 (contract-out
  [codec? (-> any/c boolean?)]
  [make-codec (-> string? codec?)]
  [codec-read (-> codec? input-port? any/c)]
  [codec-write (-> codec? any/c output-port? exact-nonnegative-integer?)]))
