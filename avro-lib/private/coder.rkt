#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/generic
         racket/match
         "varint.rkt")

(provide
 (struct-out record-field)
 (struct-out alternative))

(struct record-field (name coder))
(struct alternative (name coder))

(provide
 coder?
 coder-read
 coder-write

 null-coder
 bool-coder
 int-coder
 long-coder
 float-coder
 double-coder
 bytes-coder
 string-coder
 record-coder
 enum-coder
 array-coder
 map-coder
 union-coder
 fixed-coder
 optional-coder)

(define-generics coder
  {coder-read coder in}
  {coder-write coder v out})

(define (coder-read* c in)
  (coder-read (if (box? c) (unbox c) c) in))

(define (coder-write* c v out)
  (coder-write (if (box? c) (unbox c) c) v out))

(define-syntax (define-coder stx)
  (syntax-parse stx
    [(_ id:id (fld:id ...) #:singleton . rest)
     #'(begin
         (define-values (id)
           (let ()
             (define-coder id (fld ...) . rest)
             (values (id)))))]
    [(_ id:id (fld:id ...)
        {~seq #:read read-proc}
        {~seq #:write write-proc})
     #'(begin
         (struct id (fld ...)
           #:methods gen:coder
           [(define coder-read read-proc)
            (define coder-write write-proc)]))]))

(define-coder null-coder ()
  #:singleton
  #:read (λ (_ _in) 'null)
  #:write (λ (_ _v _out) 0))

(define-coder bool-coder ()
  #:singleton
  #:read (λ (_ in)
           (= (read-byte in) 1))
  #:write (λ (_ v out)
            (begin0 1
              (write-byte (if v 1 0) out))))

(define-coder int-coder ()
  #:singleton
  #:read (λ (_ in)
           (read-varint in))
  #:write (λ (_ v out)
            (when (or (> v #x7FFFFFFF)
                      (< v #x-80000000))
              (error 'int-coder "value must be in the range [-0x80000000, 0x7FFFFFFF], got: ~s" v))
            (write-varint v out)))

(define-coder long-coder ()
  #:singleton
  #:read (λ (_ in)
           (read-varint in))
  #:write (λ (_ v out)
            (when (or (> v #x7FFFFFFFFFFFFFFF)
                      (< v #x-8000000000000000))
              (error 'long-coder "value must be in the range [-2^63, 2^63), got: ~s" v))
            (write-varint v out)))

(define-coder float-coder ()
  #:singleton
  #:read (λ (_ in) (floating-point-bytes->real (expect-bytes 'coder-read "float" 4 in) #f))
  #:write (λ (_ v out) (write-bytes (real->floating-point-bytes v 4 #f) out)))

(define-coder double-coder ()
  #:singleton
  #:read (λ (_ in) (floating-point-bytes->real (expect-bytes 'coder-read "double" 8 in) #f))
  #:write (λ (_ v out) (write-bytes (real->floating-point-bytes v 8 #f) out)))

(define-coder bytes-coder ()
  #:singleton
  #:read (λ (_ in)
           (define len (coder-read long-coder in))
           (expect-bytes 'coder-read "bytes" len in))
  #:write (λ (_ v out)
            (+ (coder-write long-coder (bytes-length v) out)
               (write-bytes v out))))

(define-coder string-coder ()
  #:singleton
  #:read (λ (_ in) (bytes->string/utf-8 (coder-read bytes-coder in)))
  #:write (λ (_ v out) (coder-write bytes-coder (string->bytes/utf-8 v) out)))

(define-coder record-coder (fields)
  #:read (λ (r in)
           (match-define (record-coder fields) r)
           (for/hasheq ([fld (in-list fields)])
             (match-define (record-field name coder) fld)
             (values name (coder-read* coder in))))
  #:write (λ (r v out)
            (match-define (record-coder fields) r)
            (for/sum ([fld (in-list fields)])
              (match-define (record-field name coder) fld)
              (coder-write* coder (hash-ref v name) out))))

(define-coder enum-coder (variants)
  #:read (λ (e in)
           (match-define (enum-coder variants) e)
           (define variant-idx (coder-read int-coder in))
           (unless (< variant-idx (vector-length variants))
             (error 'enum-coder "enum value exceeds the number of variants: ~s" variant-idx))
           (vector-ref variants variant-idx))
  #:write (λ (e v out)
            (match-define (enum-coder variants) e)
            (define variant-idx
              (for/first ([(variant idx) (in-indexed (in-vector variants))]
                          #:when (eq? variant v))
                idx))
            (unless variant-idx
              (error 'enum-coder "enum variant not found: ~s" v))
            (coder-write int-coder variant-idx out)))

(define-coder array-coder (entry-coder)
  #:read (λ (a in)
           (match-define (array-coder entry-coder) a)
           (define (read-block len)
             (for/list ([_ (in-range len)])
               (coder-read* entry-coder in)))
           (let loop ([blocks null])
             (define len
               (coder-read long-coder in))
             (cond
               [(zero? len)
                (apply append (reverse blocks))]
               [(negative? len)
                (void (coder-read long-coder in))
                (loop (cons (read-block (abs len)) blocks))]
               [else
                (loop (cons (read-block len) blocks))])))
  #:write (λ (a v out)
            (match-define (array-coder entry-coder) a)
            (define len (length v))
            (+ (coder-write long-coder len out)
               (for/sum ([e (in-list v)])
                 (coder-write* entry-coder e out))
               (coder-write long-coder 0 out))))

(define-coder map-coder (value-coder)
  #:read (λ (m in)
           (match-define (map-coder value-coder) m)
           (define (read-block h len)
             (for/fold ([h h])
                       ([_ (in-range len)])
               (define k-str (coder-read string-coder in))
               (define v (coder-read* value-coder in))
               (hash-set h (string->symbol k-str) v)))
           (let loop ([res (hasheq)])
             (define len
               (coder-read long-coder in))
             (cond
               [(zero? len) res]
               [(negative? len)
                (void (coder-read long-coder in))
                (loop (read-block res (abs len)))]
               [else
                (loop (read-block res len))])))
  #:write (λ (m v out)
            (match-define (map-coder value-coder) m)
            (define len (hash-count v))
            (+ (coder-write long-coder len out)
               (for/sum ([(k v) (in-hash v)])
                 (define k-str (symbol->string k))
                 (+ (coder-write string-coder k-str out)
                    (coder-write* value-coder v out)))
               (coder-write long-coder 0 out))))

(define-coder union-coder (alternatives)
  #:read (λ (u in)
           (match-define (union-coder alternatives) u)
           (define alternative-idx
             (coder-read int-coder in))
           (unless (< alternative-idx (vector-length alternatives))
             (error 'union-coder "alternative index exceeds length of alts: ~s" alternative-idx))
           (match-define (alternative name coder)
             (vector-ref alternatives alternative-idx))
           (hasheq 'type name 'value (coder-read* coder in)))
  #:write (λ (u v out)
            (match-define (union-coder alternatives) u)
            (define type (hash-ref v 'type))
            (define value (hash-ref v 'value))
            (define alternative-idx
              (for/first ([(alt idx) (in-indexed (in-vector alternatives))]
                          #:when (equal? (alternative-name alt) type))
                idx))
            (unless alternative-idx
              (error 'union-coder "no alternative found for type: ~s" type))
            (define coder
              (alternative-coder
               (vector-ref alternatives alternative-idx)))
            (+ (coder-write int-coder alternative-idx out)
               (coder-write* coder value out))))

(define-coder fixed-coder (len)
  #:read (λ (f in)
           (match-define (fixed-coder len) f)
           (expect-bytes 'fixed-coder "fixed" len in))
  #:write (λ (f v out)
            (match-define (fixed-coder len) f)
            (unless (= (bytes-length v) len)
              (error 'fixed-coder "expected ~a bytes but found: ~e" len v))
            (write-bytes v out)))

(define-coder optional-coder (c default)
  #:read (λ (o in)
           (match-define (optional-coder c default) o)
           (cond
             [(eof-object? (peek-byte in)) default]
             [else (coder-read* c in)]))
  #:write (λ (o v out)
            (match-define (optional-coder c _) o)
            (coder-write* c v out)))


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expect-bytes who what n in)
  (define bs (read-bytes n in))
  (begin0 bs
    (when (or (eof-object? bs)
              (< (bytes-length bs) n))
      (error who "unexpected EOF while reading ~a" what))))
