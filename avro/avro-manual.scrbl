#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     racket/contract
                     avro))

@title{@tt{avro}: Apache Avro}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@(define (avro-link . pre-content)
  (apply link "https://avro.apache.org/docs/1.11.1/specification/" pre-content))

This package implements support for decoding and encoding data
specified using the @avro-link{Apache Avro} protocol.  It is a work in
progress and breaking changes are to be expected.

@section{Schema Evolution}

@(define (broken-link . pre-content)
   (apply link "https://github.com/linkedin/goavro/blob/8eb9f0e2d756cea165f593f80c6780f4b0b4dbb6/SCHEMA-EVOLUTION.md" pre-content))

I agree with the authors of the @tt{goavro} package that Avro schema
evolution seems @broken-link{broken}.  As such, this package doesn't
attempt to support field @tt{default}s.

@section{Missing Features}
@subsection{Aliases}

Aliasing schemas during read is not currently supported.

@subsection{JSON Encoding}

JSON encoding is not currently supported and it is unlikely it will be
unless someone else is interested in adding support.

@subsection{RPC}

I don't plan on supporting the RPC features at the moment.

@section{Reference}
@defmodule[avro]

@deftech{Codecs} are opaque values that can be used to read and write
data according to an Avro schema.

Primitive values are mapped between Avro and Racket according to the
following table:

@tabular[#:style 'boxed
         #:column-properties '(left right)
         #:row-properties '(bottom-border ())
         (list (list @bold{Avro Type}   @bold{Racket Contract})
               (list @tt{null}          @racket['null])
               (list @tt{boolean}       @racket[boolean?])
               (list @tt{int}           @racket[(integer-in (- (expt 2 31)) (sub1 (expt 2 31)))])
               (list @tt{long}          @racket[(integer-in (- (expt 2 63)) (sub1 (expt 2 63)))])
               (list @tt{float}         @racket[real?])
               (list @tt{double}        @racket[real?])
               (list @tt{bytes}         @racket[bytes?])
               (list @tt{string}        @racket[string?]))]

Records are represented by @racket[hasheq] hashes with symbols for
keys.  Enums are represented by the symbols that make up their
variants.  Unions are represented by @racket[hasheq] hashes with two
keys: @racket['type] representing the fully-qualified name of the
variant and @racket['value], containing the value.

@defproc[(codec? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{codec}.
}

@defproc[(make-codec [schema string?]) codec?]{
  Converts the given Avro schema to a @tech{codec}.  The schema must
  be a valid JSON string in Avro definition format.

  @examples[
    (require avro json)

    (define c
      (make-codec
       (jsexpr->string
        (hasheq
         'type "record"
         'name "LongList"
         'fields (list
                  (hasheq
                   'name "value"
                   'type "long")
                  (hasheq
                   'name "next"
                   'type '("null" "LongList")))))))

    (define v
      (hasheq
       'value 1
       'next (hasheq
              'type "LongList"
              'value (hasheq
                      'value 2
                      'next (hasheq
                             'type "null"
                             'value 'null)))))

    (define buf (open-output-bytes))
    (codec-write c v buf)
    (codec-read c (open-input-bytes (get-output-bytes buf)))
  ]
}

@defproc[(codec-read [c codec?]
                     [in input-port?]) any/c]{

  Reads a value from @racket[in] according to @racket[c].
}

@defproc[(codec-write [c codec?]
                      [v any/c]
                      [out output-port?]) exact-nonnegative-integer?]{

  Writes @racket[v] to @racket[out] according to @racket[c].  Returns
  the number of bytes written.
}

@subsection{Object Container Format}
@defmodule[avro/container]

@defproc[(read-container [in input-port?]) list?]{
  Reads a list of objects from @racket[in] using the Avro Object
  Container Format.
}

@defproc[(write-container [schema string?]
                          [values list?]
                          [out output-port?]
                          [#:block-size block-size exact-positive-integer? (* 100 1024 1024)]
                          [#:compression compression (or/c 'none 'deflate) 'deflate]) void?]{

  Writes @racket[values] to @racket[out] using the Avro Object
  Container Format and the given @racket[schema].  The
  @racket[block-size] argument is a hint that instructs the writer to
  start new data blocks once the size (before compression) has been
  exceeded.
}
