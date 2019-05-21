#lang racket/base

(require crypto
         crypto/libcrypto
         racket/format
         racket/match
         racket/port
         racquel
         "bidi.rkt"
         "config.rkt"
         "db.rkt")

(define current-crypto-impl (make-parameter #f))

(define (make-crypto-impl)
  (crypto-factories (list libcrypto-factory))
  (get-cipher (config-ref '(ticket-crypto cipher-spec))))

(define (ser obj)
  (string->bytes/utf-8 (~s obj)))

(define (unser str)
  (call-with-input-bytes str read))

(define (generate-mpapi-ticket user intention)
  (bytes->base64-string
   (encrypt (current-crypto-impl)
            (config-ref '(ticket-crypto key))
            (config-ref '(ticket-crypto iv))
            (ser (list (current-seconds) (get-column id user) intention)))))

;; (values user intention)
(define (get-ticket-data ticket)
  (match (unser (decrypt (current-crypto-impl)
                         (config-ref '(ticket-crypto key))
                         (config-ref '(ticket-crypto iv))
                         (base64-string->bytes ticket)))
    [(list (? integer? timestamp)
           (? integer? user-id)
           (? string? intention))
     (when (< (+ timestamp (config-ref '(ticket-crypto expires-seconds)))
              (current-seconds))
       (error "timestamp expired"))
     (values (make-data-object db user% user-id)
             intention)]))

(provide current-crypto-impl
         make-crypto-impl
         generate-mpapi-ticket
         get-ticket-data)
