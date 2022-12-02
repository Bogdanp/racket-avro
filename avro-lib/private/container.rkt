#lang racket/base

(require file/gunzip
         file/gzip
         json
         racket/port
         racket/random
         "codec.rkt"
         "coder.rkt")

(provide
 read-container
 write-container)

(define (read-container in)
  (define header
    (codec-read header-codec in))
  (define magic (hash-ref header 'magic))
  (unless (equal? magic magic-header)
    (error 'read-container "invalid magic header: ~s" magic))
  (define meta
    (hash-ref header 'meta))
  (define compression
    (case (hash-ref meta 'avro.codec #"null")
      [(#"null") 'none]
      [(#"deflate") 'deflate]
      [else (error 'read-container "unsupported compression codec: ~s" compression)]))
  (define schema
    (hash-ref meta 'avro.schema #f))
  (unless schema
    (error 'read-container "missing schema"))
  (define codec
    (make-codec (bytes->string/utf-8 schema)))
  (define sync-bs
    (hash-ref header 'sync))
  (let loop ([res-blocks null])
    (cond
      [(eof-object? (peek-byte in))
       (apply append (reverse res-blocks))]
      [else
       (define num
         (coder-read long-coder in))
       (cond
         [(negative? num)
          (error 'read-container "negative block count")]
         [(zero? num)
          (loop res-blocks)]
         [else
          (define len (coder-read long-coder in))
          (define limited-in
            (make-limited-input-port in len #f))
          (define-values (blocks-in blocks-out)
            (make-pipe))
          (thread
           (lambda ()
             (case compression
               [(none) (copy-port limited-in blocks-out)]
               [(deflate) (inflate limited-in blocks-out)])
             (close-output-port blocks-out)))
          (define blocks
            (for/list ([_ (in-range num)])
              (codec-read codec blocks-in)))
          (define block-sync-bs
            (read-bytes 16 in))
          (unless (equal? sync-bs block-sync-bs)
            (error 'read-container "invalid sync block: ~s" block-sync-bs))
          (loop (cons blocks res-blocks))])])))

(define (write-container schema vs out
                         #:block-size [block-size (* 100 1024 1024)]
                         #:compression [compression 'deflate])
  (define codec (make-codec schema))
  (define compression-bs
    (case compression
      [(none) #"null"]
      [(deflate) #"deflate"]
      [else (raise-argument-error 'write-container "(or/c 'none 'deflate)" compression)]))
  (define sync-bs
    (crypto-random-bytes 16))
  (define header
    (hash
     'magic magic-header
     'meta (hasheq
            'avro.schema (string->bytes/utf-8 schema)
            'avro.codec compression-bs)
     'sync sync-bs))
  (codec-write header-codec header out)
  (define buf (open-output-bytes))
  (define (flush! num len)
    (coder-write long-coder num out)
    (unless (zero? num)
      (define bs (get-output-bytes buf #t))
      (define-values (compressed-bs compressed-len)
        (case compression
          [(none)
           (values bs len)]
          [(deflate)
           (define compressed-bs-out (open-output-bytes))
           (define-values (_nread nbytes _crc)
             (deflate (open-input-bytes bs) compressed-bs-out))
           (values (get-output-bytes compressed-bs-out) nbytes)]))
      (coder-write long-coder compressed-len out)
      (void (write-bytes compressed-bs out))
      (void (write-bytes sync-bs out))))
  (let loop ([lst vs] [num 0] [len 0])
    (cond
      [(null? lst)
       (flush! num len)]
      [(< block-size len)
       (flush! num len)
       (loop lst 0 0)]
      [else
       (define nbytes
         (codec-write codec (car lst) buf))
       (loop (cdr lst) (add1 num) (+ len nbytes))])))

(define magic-header
  #"Obj\1")

(define header-codec
  (make-codec
   (jsexpr->string
    (hasheq
     'type "record"
     'name "org.apache.avro.file.Header"
     'fields (list
              (hasheq
               'name "magic"
               'type (hasheq 'type "fixed" 'name "Magic" 'size 4))
              (hasheq
               'name "meta"
               'type (hasheq 'type "map" 'values "bytes"))
              (hasheq
               'name "sync"
               'type (hasheq 'type "fixed" 'name "Sync" 'size 16)))))))
