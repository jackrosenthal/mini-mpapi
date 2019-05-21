#lang racket/base

(require net/sendmail
         "config.rkt")

(define (sendmail to subject body)
  (send-mail-message (config-ref '(mail from))
                     subject
                     (list to)
                     '() '()
                     body))

(provide sendmail)
