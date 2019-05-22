#lang racket/base

(require net/base64
         racket/string
         web-server/dispatch/bidi-match
         web-server/dispatch/coercion)

(define-syntax define-bidi-match-expander/coercions
  (syntax-rules ()
    [(_ id in-test? in out-test? out)
     (begin (define-coercion-match-expander in/m in-test? in)
            (define-coercion-match-expander out/m out-test? out)
            (define-bidi-match-expander id in/m out/m))]))

(define (base64-string->bytes s)
  (base64-decode (string->bytes/utf-8
                  (string-replace
                   (string-replace s "-" "/")
                   "_" "+"))))

(define (bytes->base64-string b)
  (string-replace
   (string-replace
    (bytes->string/utf-8 (base64-encode b #""))
    "/" "-")
   "+" "_"))

(define base64-string? (make-coerce-safe? base64-string->bytes))

(define-bidi-match-expander/coercions base64-arg
  base64-string? base64-string->bytes
  bytes? bytes->base64-string)

(provide base64-arg
         bytes->base64-string
         base64-string->bytes)
