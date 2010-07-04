(module check mzscheme

  (define version-url "http://download.plt-scheme.org/version")
  (define timeout 30)

  ;; This file is invoked from the installer, without using zo files -- so
  ;; using net/url.ss is extremely slow.  Instead, do things directly.
  ;; (require (lib "url.ss" "net"))
  ;; (define (url->port url)
  ;;   (get-pure-port (string->url url)))
  (define (url->port url)
    (define-values (host path)
      (apply values (cdr (regexp-match #rx"^http://([^/:@]+)(/.*)$" url))))
    (define-values (i o) (tcp-connect host 80))
    (fprintf o "GET ~a HTTP/1.0\r\nHost: ~a\r\n\r\n" path host)
    (flush-output o)
    (close-output-port o)
    (unless (regexp-match #rx"^HTTP/[0-9.]+ 200 OK\r\n.*?\r\n\r\n" i)
      (error 'url->port "bad reply from server: ~a" (read-line)))
    i)

  (define error-value
    (case-lambda
     [(what) `(error ,what)]
     [(what more)
      `(error ,what
              ,(cond [(list? more) (format "~a" more)]
                     [(exn? more)  (format "(~a)" (exn-message more))]
                     [else         (format "(~a)" more)]))]))

  (define (with-timeout timeout thunk)
    (define result #f)
    (let ([r (sync/timeout timeout
               (thread (lambda ()
                         (set! result
                               (with-handlers
                                   ([void (lambda (e)
                                            (error-value "internal error" e))])
                                 (thunk))))))])
      (if r result (error-value "timeout"))))

  (define (check-version/timeout)
    (let/ec escape
      (define (err . args) (escape (apply error-value args)))
      (define-syntax try
        (syntax-rules ()
          [(_ expr error-message)
           (with-handlers ([void (lambda (e) (err error-message e))]) expr)]))
      ;; Get server information, carefully
      (define version-info
        (parameterize ([current-input-port
                        (try (url->port version-url)
                             "could not connect to website")])
          (try (read) "unexpected response from server")))
      (define (get key)
        (cond [(assq key version-info) => cadr]
              [else (err (format "no `~s' in response" key) version-info)]))
      (unless (and (list? version-info)
                   (andmap (lambda (x)
                             (and (list? x)
                                  (= 2 (length x))
                                  (symbol? (car x))
                                  (string? (cadr x))))
                           version-info))
        (err "bad response from server" version-info))
      ;; Make a decision
      (let ([current (version)]
            [stable (get 'stable)]
            [recent (get 'recent)])
        (cond
         ;; we have the newest version (can be > if we have an svn build)
         [(string>=? current recent) 'ok]
         ;; we're stable, but there's a newer version
         [(equal? current stable)
          `(ok-but ,recent)]
         ;; new version out -- no alphas or we have an alpha => show recent
         ;; (also for svn builds of a stable version -- anything with ".")
         [(or (equal? recent stable)
              (and (regexp-match #rx"[.]" current)
                   ;; but if we have an alpha that is older then the current
                   ;; stable then go to the next case
                   (string>=? current stable)))
          `(newer ,recent)]
         ;; new version out, we have an outdated stable, there is also an alpha
         ;; (alternatively, we have an alpha that is older than the current
         ;; stable)
         [else `(newer ,stable ,recent)]))))

  ;; Check the version on the server and compare to our version.
  ;; Possible return values (message is always a string):
  ;; * `ok
  ;;   You're fine.
  ;; * `(ok-but ,version)
  ;;   You have a fine stable version, but note that there is a newer alpha
  ;; * `(newer ,version)
  ;;   You have an old version, please upgrade to `version'
  ;; * `(newer ,version ,alpha)
  ;;   You have an old version, please upgrade to `version' you may consider
  ;;   also the alpha version
  ;; * `(error ,message [,additional-info])
  ;;   An error occured, the third (optional) value can be shown as the system
  ;;   error that happened or the value that caused an error.
  (provide check-version)
  (define (check-version)
    (with-timeout timeout check-version/timeout))

  )
