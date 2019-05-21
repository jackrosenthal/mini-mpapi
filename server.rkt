#lang racket/base

(require (only-in db
                  query-exec)
         json
         net/dns
         net/url-string
         racket/class
         racket/cmdline
         racket/match
         racket/port
         racket/random
         racquel
         web-server/dispatch
         web-server/formlets
         web-server/http
         web-server/http/bindings
         web-server/http/cookie
         web-server/http/id-cookie
         web-server/http/redirect
         web-server/http/response-structs
         web-server/http/status-code
         web-server/http/xexpr
         web-server/page
         web-server/servlet-env
         "bidi.rkt"
         "config.rkt"
         "db.rkt"
         "formlets.rkt"
         "mail.rkt"
         "template.rkt"
         "tickets.rkt")

(define nameserver (dns-find-nameserver))

(define (dns-get-ip host)
  (dns-get-address nameserver host))

(define (return-url-allowed? url)
  (or (member (url-host url) (config-ref '(allowed-hosts)))
      (let ((addr (dns-get-ip (url-host url))))
          (memf (λ (host)
                  (equal? addr (dns-get-ip host)))
                (config-ref '(allowed-systems))))))

(define (respond/error code [fstring ""] . args)
  (let* ([status-msg (message-for-status-code code)]
         [errmsg (format "~a: ~a" code status-msg)])
    (raise
     (response/xexpr
      (template (hasheq 'title errmsg)
                `(p ,(apply format fstring args)))
      #:code code))))

(define current-request (make-parameter #f))

(define-syntax define-servlet
  (syntax-rules ()
    [(_ (name lambda-list ...) body ...)
     (define (name request lambda-list ...)
       (with-handlers ([response? (λ (r) r)])
         (parameterize ([current-request request])
           body ...)))]))

(define (required-binding name [parser (λ (x) x)])
  (let ([bindings (request-bindings (current-request))])
    (cond
      [(not (exists-binding? name bindings))
       (respond/error
        400
        "The required parameter \"~A\" was not supplied."
        name)]
      [else (with-handlers
              ([exn:fail?
                (λ (e)
                  (respond/error
                   400
                   "The supplied parameter \"~A\" was not correctly formatted."
                   name))])
              (parser (extract-binding/single name bindings)))])))

(define (current-user)
  (let ([id (request-id-cookie (current-request)
                               #:name "user"
                               #:key (config-ref '(secret-salt))
                               #:shelf-life (config-ref '(session-life)))])
    (and id (make-data-object db user% (string->number id)))))

(define (create-session user)
  (make-id-cookie "user" (number->string (get-column id user))
                  #:key (config-ref '(secret-salt))
                  #:max-age (config-ref '(session-life))))

(define-servlet (handle-sso)
  (let ([return-url (required-binding 'return string->url)])
    (cond
      [(not (return-url-allowed? return-url))
       (respond/error 403 "The application you are using is not configured
to use this authentication server.")]
      [else (let ([u (current-user)])
              (if u
                  (handle-return #:user u)
                  (redirect-to
                   (url-generator account-login-page)
                   #:headers
                   (list (cookie->header
                          (make-cookie "continue"
                                       (url->string return-url)
                                       #:max-age
                                       (config-ref '(session-life))))))))])))

(define (login-formlet [err #f])
  (formlet
   (div
    ,@(if err
          `((div ([class "alert alert-danger"])
                 ,err))
          '())
    ,{(form-group (bs-email-input) #:label "Email") . => . email}
    ,{(form-group (bs-password-input) #:label "Password") . => . password}
    (p ([class "mb-3"])
       (a ([href ,(url-generator reset-password-page)])
          "Forgot password?"))
    ,{=> (bs-submit
          "Login"
          #:attributes '((class "btn btn-primary btn-lg btn-block")))
         unused})
   (let* ([email (bytes->string/utf-8 (binding:form-value email))]
          [password (binding:form-value password)]
          [u (user-from-email email)])
     (cond
       [(not u) (account-login-page (current-request)
                                    "Email not registered on system")]
       [(not (send u check-password password))
        (page
         (account-login-page
          (current-request)
          `(span "Invalid password. "
                 (a ([href ,(embed/url (send-reset-email/f u))])
                    "Want to reset it?"))))]
       [else (handle-return
              #:user u
              #:cookies (list (create-session u)))]))))

(define-servlet (account-login-page [err #f])
  (send/formlet (login-formlet err)
                #:wrap (make-wrapper (hasheq 'title "Login"))))

(define (complete-request-uri req)
  (match (request-uri req)
    [(struct url (scheme user host port path-absolute? path query fragment))
     (make-url (config-ref '(application scheme))
               user
               (config-ref '(application host))
               (config-ref '(application port))
               path-absolute?
               path
               query
               fragment)]))

(define (send-reset-email/f user)
  (λ (req)
    (define token (crypto-random-bytes 12))
    (define reset (new password-reset%))
    (set-column! token reset token)
    (set-column! user-id reset (get-column id user))
    (sendmail (get-column email user)
              "Password Reset Request"
              (list "Hello,"
                    ""
                    "Here is a link to reset your password:"
                    (url->string
                     (combine-url/relative
                      (complete-request-uri req)
                      (url-generator new-password-handler token)))))
    (insert-data-object db reset)
    (response/xexpr
     (template (hasheq 'title "Reset Requested!")
               '(p "Check your email for a link to reset your password.")))))

(define (reset-servlet [err #f])
  (formlet
   (div
    ,@(if err
          `((div ([class "alert alert-danger"])
                 ,err))
          '())
    (p "Enter the email account you are registered with to send a reset email.")
    ,(=> (form-group (bs-email-input) #:label "Email") email)
    ,(=> (bs-submit "Continue") unused))
   (let* ([email (bytes->string/utf-8 (binding:form-value email))]
          [u (user-from-email email)])
     (cond
       [(not u) (reset-password-page
                 (current-request)
                 "That email is not registered.")]
       [else ((send-reset-email/f u) (current-request))]))))

(define-servlet (reset-password-page [err #f])
  (send/formlet (reset-servlet err)
                #:wrap (make-wrapper (hasheq 'title "Reset Password"))))

(define (new-password user [err #f])
  (formlet
   (div
    ,@(if err
          `((div ([class "alert alert-danger"])
                 ,err))
          '())
    ,(=> (form-group (bs-password-input)
                     #:label "New Password"
                     #:help
                     '(span "Must be at least 10 characters. "
                            (a ([href "https://www.random.org/passwords/?num=5&len=21&format=html&rnd=new"]
                                [target "_blank"]
                                [class "mb-3"])
                               "Need a suggestion?")))
         password)
    ,(=> (bs-submit "Set Password"
                    #:attributes '([class "btn btn-primary btn-lg btn-block"]))
         ignore))
   (let ([password (bytes->string/utf-8 (binding:form-value password))])
     (cond
       [(< (string-length password) 10)
        (send/formlet
         (new-password
          user
          "The password you have selected does not meet requirements.")
         #:wrap (make-wrapper (hasheq 'title "New Password")))]
       [else
        (send user set-password! password)
        (query-exec db
                    "delete from password_resets where user_id = $1"
                    (get-column id user))
        (handle-return
         #:user user
         #:cookies (list (create-session user)))]))))

(define-servlet (new-password-handler token)
  (let* ([reset (with-handlers
                  ([exn:fail? (λ (e)
                                (respond/error 403 "Invalid reset token"))])
                  (make-data-object db password-reset% token))]
         [user (make-data-object db user% (get-column user-id reset))])
    (send/formlet (new-password user)
                  #:wrap (make-wrapper (hasheq 'title "New Password")))))

(define (append-query uri alist)
  (match uri
    [(struct url (scheme user host port path-absolute? path query fragment))
     (make-url scheme
               user
               host
               port
               path-absolute?
               path
               (append query alist)
               fragment)]))

(define (handle-return [req (current-request)]
                       #:user user
                       #:cookies [cookies '()])
  (define client-cookies (request-cookies req))
  (define continue-cookie (findf (λ (c)
                                   (string=? "continue" (client-cookie-name c)))
                                 client-cookies))
  (cond
    [continue-cookie
     (define continue (client-cookie-value continue-cookie))
     (redirect-to
      (url->string
       (append-query
        (string->url continue)
        `((tkt . ,(generate-mpapi-ticket user continue)))))
      see-other
      #:headers (map cookie->header cookies))]
    [else (respond/error
           400
           "I don't know where to send you. Try logging into the application you think you came from.")]))

(define-servlet (handle-fetch)
  (define-values (user intention)
    (required-binding 'tkt get-ticket-data))
  (response/output
   (jsexpr->bytes
    (hasheq 'result "success"
            'uid (get-column email user)
            'attributes (hasheq 'uidNumber (get-column id user)
                                'username (get-column email user)
                                'first (get-column email user)
                                'sn ""
                                'email (get-column email user))
            'intention intention))))

(define-servlet (handle-slo)
  (response/xexpr
   (template (hasheq 'title "Logged Out")
             '(p "For security, please close this browser tab."))
   #:headers (list (cookie->header (logout-id-cookie "user")))))

(define-values (dispatcher url-generator)
  (dispatch-rules
   [("sso") #:method "get" handle-sso]
   [("account") account-login-page]
   [("account" "reset") reset-password-page]
   [("account" "reset" (base64-arg)) new-password-handler]
   [("fetch") handle-fetch]
   [("slo") handle-slo]))

(define (start-server)
  (current-data-source (make-data-source))
  (current-crypto-impl (make-crypto-impl))
  (apply/config serve/servlet
                '(server)
                dispatcher
                #:servlet-regexp #rx""
                #:launch-browser? #f))

(module+ main
  (void
   (command-line
    #:program "mini-mpapi-server"
    #:once-each
    [("-p" "--port")
     num
     "Port number"
     (config-set! '(server port) (string->number num))]
    [("-l" "--listen-ip")
     address
     "Listen on this address"
     (config-set! '(server listen-ip) address)]
    #:multi
    [("-c" "--config")
     filename
     "Config file to extend"
     (call-with-input-file filename extend-config/port!)]
    [("-o" "--option")
     keyname val
     "Set a config option"
     (config-set! (keyname->path keyname)
                  (call-with-input-string val read))]
    #:args ()
    (start-server))))
