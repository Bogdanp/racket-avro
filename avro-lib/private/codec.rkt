#lang racket/base

(require json
         racket/format
         racket/match
         racket/port
         racket/string
         "coder.rkt")

(provide
 codec?
 make-codec
 codec-read
 codec-write)

(struct codec (name coder))

(define (make-codec schema)
  (define-values (name coder)
    (parse-schema (call-with-input-string schema read-json)))
  (codec name coder))

(define (codec-read c in)
  (coder-read (codec-coder c) in))

(define (codec-write c v out)
  (coder-write (codec-coder c) v out))

(define (parse-schema e)
  (match e
    [(? string?)
     (values e (lookup-type e))]
    [(? list?)
     (define the-coder
       (union-coder
        (for/vector ([schema (in-list e)])
          (define-values (name coder)
            (parse-schema schema))
          (unless name
            (error 'parse-schema "unions are not allowed as immediate children of other unions"))
          (alternative name coder))))
     (values #f the-coder)]
    [(? hash?)
     (define type
       (ref e 'type))
     (case type
       [("record")
        (define-values (namespace full-name)
          (ref-name e))
        (define coder-box (box #f))
        (parameterize ([current-type-namespace namespace]
                       [current-types (hash-set (current-types) full-name coder-box)])
          (define coder
            (record-coder
             (for/list ([field-schema (in-list (ref e 'fields))])
               (define field-name (string->symbol (ref field-schema 'name)))
               (define-values (_ field-coder)
                 (parse-schema (ref field-schema 'type)))
               (record-field field-name field-coder))))
          (set-box! coder-box coder)
          (values full-name coder))]
       [("enum")
        (define-values (namespace full-name)
          (ref-name e))
        (define coder-box (box #f))
        (parameterize ([current-type-namespace namespace]
                       [current-types (hash-set (current-types) full-name coder-box)])
          (define coder (enum-coder (list->vector (map string->symbol (ref e 'symbols)))))
          (set-box! coder-box coder)
          (values full-name coder))]
       [("array")
        (define-values (_ item-coder)
          (parse-schema (ref e 'items)))
        (values #f (array-coder item-coder))]
       [("map")
        (define-values (_ value-coder)
          (parse-schema (ref e 'values)))
        (values #f (map-coder value-coder))]
       [("fixed")
        (define-values (_namespace full-name)
          (ref-name e))
        (values full-name (fixed-coder (ref e 'size)))]
       [else
        (values #f (lookup-type type))])]))

(define current-type-namespace
  (make-parameter #f))

(define current-types
  (make-parameter (hash)))

(define (lookup-type name)
  (cond
    [(lookup-primitive-type name)]
    [else
     (define (fail)
       (error 'lookup-type "unknown type: ~a" name))
     (hash-ref (current-types) name fail)]))

(define (lookup-primitive-type name)
  (case name
    [("null") null-coder]
    [("boolean") bool-coder]
    [("int") int-coder]
    [("long") long-coder]
    [("float") float-coder]
    [("double") double-coder]
    [("bytes") bytes-coder]
    [("string") string-coder]
    [else #f]))

(define (ref ht id [default (λ () (error 'ref "required field ~a not found" id))])
  (hash-ref ht id default))

(define (ref-name ht)
  (define-values (namespace name)
    (let ([name (ref ht 'name)])
      (if (string-contains? name ".")
          (split-namespace name)
          (values (ref ht 'namespace (current-type-namespace)) name))))
  (if namespace
      (values namespace (~a namespace "." name))
      (values #f name)))

(define (split-namespace full-name)
  (define parts (reverse (string-split full-name ".")))
  (define name (car parts))
  (define namespace (string-join (reverse (cdr parts)) "."))
  (values namespace name))
