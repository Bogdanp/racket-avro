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

@subsection{Object Container Format}

I plan to implement support for the container format shortly.

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
variants.  Unions are represented by a pair of the fully-qualified
type name of the variant and the value.

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
       'next (cons "LongList"
              (hasheq
               'value 2
               'next (cons "null" 'null)))))

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
                      [out output-port?]) void?]{

  Writes @racket[v] to @racket[out] according to @racket[c].
}
