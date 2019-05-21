#lang racket/base

(require racket/function
         txexpr
         web-server/formlets)

(define-syntax-rule (when/list predicate body ...)
  (if predicate
      (list body ...)
      '()))

(define-syntax-rule (unless/list predicate body ...)
  (when/list (not predicate)
             body ...))

(define (make-id)
  (symbol->string (gensym "id-")))

(define (form-group element
                    #:label [label-text #f]
                    #:help [help-text #f])
  (let* ([xexpr (car (formlet-display element))]
         [attrs (get-attrs xexpr)]
         [id-pair (assq 'id attrs)]
         [id (if id-pair (cadr id-pair) (make-id))]
         [help-id-pair (assq 'aria-describedby attrs)]
         [help-id (if help-id-pair (cadr help-id-pair) (make-id))])
    (formlet
     (div ([class "form-group"])
          ,@(when/list label-text
                       (if id-pair
                           `(label ([for ,id]) ,label-text)
                           `(label ,label-text)))
          ,(element . => . value)
          ,@(when/list help-text
                       `(small ([id ,help-id]
                                [class "form-text text-muted"])
                               ,help-text)))
     value)))

(define (maybe-add-attributes attrs . to-add)
  (for/fold ([attrs attrs])
            ([pair (in-list to-add)])
    (if (assq (car pair) attrs)
        attrs
        (cons pair attrs))))

(define bs-input
  (make-keyword-procedure
   (λ (kws kwds type #:attributes [attributes '()] . args)
     (keyword-apply input kws kwds '()
                    #:type type
                    #:attributes (maybe-add-attributes
                                  attributes
                                  `(id ,(make-id))
                                  '(class "form-control"))))))

(define bs-text-input (curry bs-input "text"))
(define bs-password-input (curry bs-input "password"))
(define bs-email-input (curry bs-input "email"))

(define (bs-submit value #:attributes [attributes '()])
  (submit value
          #:attributes (maybe-add-attributes
                        attributes
                        '(class "btn btn-primary"))))

(provide form-group
         bs-input
         bs-text-input
         bs-email-input
         bs-password-input
         bs-submit)
