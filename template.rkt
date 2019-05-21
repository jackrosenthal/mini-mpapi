#lang racket/base

(require css-expr)

(provide current-bootstrap-url
         template
         make-wrapper)

(define current-bootstrap-url
  (make-parameter
   "https://stackpath.bootstrapcdn.com/bootswatch/4.3.1/journal/bootstrap.min.css"))

(define (template attributes main-body)
  `(html (head (title ,(hash-ref attributes 'title "Login"))
               (meta ([name "viewport"]
                      [content "width=device-width, initial-scale=1"]))
               (link ([href ,(current-bootstrap-url)]
                      [rel "stylesheet"]))
               (style ,(css-expr->css
                        (css-expr
                         [body #:background-color |#eee|]
                         [.container #:max-width 480px
                                     #:box-shadow (0px 3px 10px |#777|)
                                     #:border-radius 5px
                                     #:padding 0px
                                     #:margin-top 20px
                                     #:background-color white]
                         [.container-title #:display block
                                           #:width 100%
                                           #:text-align center
                                           #:font-size 24px
                                           #:font-weight bold
                                           #:padding 20px
                                           #:border-top-left-radius 5px
                                           #:border-top-right-radius 5px
                                           #:color white]
                         [.container-body #:display block
                                          #:width 100%
                                          #:padding 15px]))))
         (body
          (div ([class "container"])
               (div ([class "container-title bg-primary"])
                    ,(hash-ref attributes 'title "Login"))
               (div ([class "container-body"])
                    ,main-body)))))

(define (make-wrapper attributes)
  (λ (main-body)
    (template attributes main-body)))
