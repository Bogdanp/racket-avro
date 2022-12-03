#lang racket/base

(require avro/container
         json
         racket/list
         racket/runtime-path
         rackunit)

(define-runtime-path examples "examples")

(define (read-container-file path)
  (call-with-input-file path read-container))

(define example-schema
  (jsexpr->string
   (hasheq
    'type "record"
    'name "Person"
    'fields (list
             (hasheq
              'name "name"
              'type "string")
             (hasheq
              'name "age"
              'type "int")))))

(define-check (check-roundtrip schema vs compression bs)
  (define buf (open-output-bytes))
  (write-container
   #:compression compression
   #:block-size bs
   schema vs buf)
  (define read-vs
    (read-container (open-input-bytes (get-output-bytes buf))))
  (with-check-info
    (['value read-vs]
     ['expected vs])
    (unless (equal? read-vs vs)
      (fail-check))))

(define container-suite
  (test-suite
   "container"

   (check-exn
    #rx"unexpected EOF"
    (λ () (read-container-file (build-path examples "bad-header.avro"))))

   (test-case "roundtrip empty"
     (for ([compression (in-list '(none deflate))])
       (check-roundtrip example-schema null compression 1024)))

   (test-case "roundtrip one"
     (for ([compression (in-list '(none deflate))])
       (check-roundtrip example-schema (list (hasheq 'name "Bogdan" 'age 30)) compression 1024)))

   (test-case "roundtrip many"
     (define lst
       (make-list 1000 (hasheq 'name "Bogdan" 'age 30)))
     (for* ([compression (in-list '(none deflate))]
            [block-size (in-list '(1 128 1024 10240 102400))])
       (check-roundtrip example-schema lst compression block-size)))

   ;; The following examples were taken from:
   ;;   https://github.com/linkedin/goavro/tree/8eb9f0e2d756cea165f593f80c6780f4b0b4dbb6/fixtures
   (test-suite
    "goavro-fixtures"

    (check-equal? (length (read-container-file (build-path examples "quickstop-deflate.avro"))) 6001)
    (check-equal? (length (read-container-file (build-path examples "quickstop-null.avro"))) 6001)
    (check-equal? (read-container-file (build-path examples "first-block-count-not-greater-than-0.avro")) null)
    (check-equal? (read-container-file (build-path examples "second-block-count-0.avro")) '(-49))
    (check-exn
     #rx"invalid sync block"
     (λ () (read-container-file (build-path examples "sync-market-mismatch.avro")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests container-suite))
