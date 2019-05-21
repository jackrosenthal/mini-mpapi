#lang racket/base

(require racket/contract
         racket/format
         racket/hash
         racket/match
         racket/string)

(define current-config (make-parameter (hasheq)))

(define (alist? obj)
  (match obj
    [(list (cons (? symbol?) _) ...) #t]
    [_ #f]))

(define (alist->hasheq/nested alist)
  (for/hasheq ([pair (in-list alist)])
    (match pair
      [(cons name val)
       (values name
               (cond
                 [(alist? val)
                  (alist->hasheq/nested val)]
                 [(memq val '(null nil)) '()]
                 [else val]))])))

(define (hash-union/nested a b)
  (hash-union a b
              #:combine/key
              (λ (key a-value b-value)
                (match* (a-value b-value)
                  [((? hash?) (? hash?))
                   (hash-union/nested a-value b-value)]
                  [(_ _) b-value]))))

(define/contract (extend-config! new-alist)
  (-> alist? void?)
  (current-config
   (hash-union/nested (current-config)
                      (alist->hasheq/nested new-alist))))

(define/contract (extend-config/port! port)
  (-> input-port? void?)
  (extend-config! (begin0
                      (read port)
                    (unless (eof-object? (read port))
                      (error "Extra data at end of config")))))

(define (config-ref path [config (current-config)])
  (match path
    [(list) config]
    [(list-rest (? symbol? fst) rst)
     (config-ref rst (hash-ref config fst))]))

(define (config-set! path value)
  (define (rec path)
    (match path
      [(list) value]
      [(list-rest (? symbol? fst) rst)
       `((,fst . ,(rec rst)))]))
  (extend-config! (rec path)))

(define (symbol->keyword sym)
  (string->keyword (symbol->string sym)))

(define (keyword-apply/hash proc kwhash args)
  (let* ([unsorted (for/list ([(kw val) (in-hash kwhash)])
                     (list kw val))]
         [sorted (sort unsorted
                       (λ (a b)
                         (keyword<? (car a) (car b))))]
         [keywords (map car sorted)]
         [kwargs (map cadr sorted)])
    (keyword-apply proc keywords kwargs args)))

(define apply/config
  (make-keyword-procedure
   (λ (kws kwargs proc path . args)
     (let ([h (make-hasheq)])
       (for ([(sym val) (in-hash (config-ref path))])
         (hash-set! h (symbol->keyword sym) val))
       (for ([kw kws]
             [kwarg kwargs])
         (hash-set! h kw kwarg))
       (keyword-apply/hash proc h args)))))

(define (devel-mode?)
  (config-ref '(devel-mode)))

;; convert from a string like a/b/c to '(a b c)
(define (keyname->path keyname)
  (map string->symbol
       (string-split keyname "/")))

(define (config->html [config (current-config)])
  (if (hash? config)
      `(ul ,@(for/list ([(k v) (in-hash config)])
               `(li (tt ,(symbol->string k))
                    ": "
                    ,(config->html v))))
      `(tt ,(~v config))))

(provide current-config
         extend-config!
         extend-config/port!
         config-ref
         config-set!
         apply/config
         devel-mode?
         keyname->path
         config->html)
