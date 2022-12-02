#lang racket/base

(require avro
         rackunit)

(define-check (check-roundtrip c v)
  (let ([buf (open-output-bytes)])
    (codec-write c v buf)
    (check-equal? (codec-read c (open-input-bytes (get-output-bytes buf))) v)))

(define codec-suite
  (test-suite
   "codec"

   (test-case "spec example"
     ;; Example from https://avro.apache.org/docs/1.11.1/specification/
     (define c
       (make-codec #<<EOF
{
  "type": "record",
  "name": "LongList",
  "fields" : [
    {"name": "value", "type": "long"},
    {"name": "next", "type": ["null", "LongList"]}
  ]
}
EOF
                   ))

     (define v
       (hasheq 'value 1
               'next (cons "LongList"
                           (hasheq 'value 2
                                   'next (cons "null" 'null)))))
     (check-roundtrip c v))

   (test-suite
    "primitives"

    (test-suite
     "null"
     (check-roundtrip (make-codec "\"null\"") 'null))

    (test-suite
     "bool"

     (let ([c (make-codec "\"boolean\"")])
       (check-roundtrip c #t)
       (check-roundtrip c #f)))

    (test-suite
     "int"

     (let ([c (make-codec "\"int\"")])
       (for ([i (in-range -10 10)])
         (check-roundtrip c i))
       (check-roundtrip c #x7FFFFFFF)
       (check-roundtrip c #x-80000000)
       (check-exn
        #rx"value must be in the range"
        (λ () (codec-write c (expt 2 32) (open-output-bytes))))))

    (test-suite
     "long"

     (let ([c (make-codec "\"long\"")])
       (for ([i (in-range -10 10)])
         (check-roundtrip c i))
       (check-roundtrip c #x7FFFFFFFFFFFFFFF)
       (check-roundtrip c #x-8000000000000000)
       (check-exn
        #rx"value must be in the range"
        (λ () (codec-write c (expt 2 64) (open-output-bytes))))
       (check-exn
        #rx"value must be in the range"
        (λ () (codec-write c (- (expt 2 64)) (open-output-bytes))))))

    (test-suite
     "float"

     (let ([c (make-codec "\"float\"")])
       (check-roundtrip c 1.0)
       (check-roundtrip c -1.0)
       (check-roundtrip c +inf.0)
       (check-roundtrip c -inf.0)))

    (test-suite
     "double"

     (let ([c (make-codec "\"double\"")])
       (check-roundtrip c 1.0)
       (check-roundtrip c -1.0)
       (check-roundtrip c +inf.0)
       (check-roundtrip c -inf.0)))

    (test-suite
     "bytes"

     (let ([c (make-codec "\"bytes\"")])
       (check-roundtrip c #"")
       (check-roundtrip c #"hello")))

    (test-suite
     "string"

     (let ([c (make-codec "\"string\"")])
       (check-roundtrip c "")
       (check-roundtrip c "hello"))))

   (test-case "enum"
     (let ([c (make-codec #<<EOF
{
  "name": "AnEnum",
  "type": "enum",
  "symbols": ["foo", "bar"]
}
EOF
)])
       (check-roundtrip c 'foo)
       (check-roundtrip c 'bar)))

   (test-case "array"
     (let ([c (make-codec #<<EOF
{
  "type": "array",
  "items": {
    "name": "Person",
    "type": "record",
    "fields": [
      {"name": "name", "type": "string"},
      {"name": "age", "type": "int"}
    ]
  }
}
EOF
                          )])
       (check-roundtrip c null)
       (check-roundtrip c (list (hasheq 'name "Bogdan" 'age 30)))))

   (test-case "fixed"
     (let ([c (make-codec #<<EOF
{
  "type": "fixed",
  "size": 16,
  "name": "md5"
}
EOF
)])
       (check-roundtrip c #"1234567890000000")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests codec-suite))
