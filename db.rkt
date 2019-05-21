#lang racket/base

(require (prefix-in kdf/ crypto)
         db
         racket/class
         racquel
         "config.rkt")

(define current-data-source (make-parameter #f))

(define (make-data-source)
  (apply/config postgresql-data-source
                '(database)))

(define (connect!)
  (dsn-connect (current-data-source)))

(define db
  (virtual-connection connect!))

(define current-kdf-algorithm (make-parameter 'scrypt))
(define current-kdf-config (make-parameter '((ln 14) (p 1) (r 8))))

(define (make-pwhash pw)
  (kdf/pwhash (current-kdf-algorithm) pw (current-kdf-config)))

(define (pwhash-verify pw pwhash)
  (if (sql-null? pwhash)
      #f
      (kdf/pwhash-verify #f pw pwhash)))

(define user%
  (data-class object%
              (table-name "users")
              (column (id #f "id")
                      (email #f "email")
                      (pwhash #f "pwhash"))
              (primary-key id #:autoincrement "users_id_seq")

              (define/public (check-password pw)
                (let check ([pw pw])
                  (cond
                    [(string? pw) (check (string->bytes/utf-8 pw))]
                    [(bytes? pw) (pwhash-verify
                                  pw (get-column pwhash this))])))

              (define/public (set-password! pw)
                (let setpw! ([pw pw])
                  (cond
                    [(string? pw) (setpw! (string->bytes/utf-8 pw))]
                    [(bytes? pw)
                     (set-column! pwhash this (make-pwhash pw))])))

              (super-new)))

(define password-reset%
  (data-class object%
              (table-name "password_resets")
              (column (token #f "token")
                      (user-id #f "user_id"))
              (primary-key token)
              (join [user user% #:cardinality 'one-to-many
                          (where (= (password-reset% user-id)
                                    (user% id)))])
              (super-new)))

(define (user-from-email email)
  (define users (select-data-objects db user%
                                     (where (= (user% email) ?))
                                     email))
  (and (pair? users) (car users)))

(provide make-data-source
         current-data-source
         user-from-email
         db
         user%
         password-reset%)
