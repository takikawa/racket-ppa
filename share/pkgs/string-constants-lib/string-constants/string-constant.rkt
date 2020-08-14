#lang racket/base
(require (for-syntax racket/base 
                     racket/list
                     (prefix-in english-ct: "private/english-string-constants.rkt"))
         racket/file
         racket/contract/base
         setup/getinfo)

(require (prefix-in english: "private/english-string-constants.rkt")
         (prefix-in german: "private/german-string-constants.rkt")
         (prefix-in french: "private/french-string-constants.rkt")
         (prefix-in danish: "private/danish-string-constants.rkt")
         (prefix-in portuguese: "private/portuguese-string-constants.rkt")
         (prefix-in japanese: "private/japanese-string-constants.rkt")
         (prefix-in traditional-chinese: "private/traditional-chinese-string-constants.rkt")
         (prefix-in simplified-chinese: "private/simplified-chinese-string-constants.rkt")
         (prefix-in bulgarian: "private/bulgarian-string-constants.rkt")
         (prefix-in russian: "private/russian-string-constants.rkt")
         (prefix-in ukrainian: "private/ukrainian-string-constants.rkt"))

(provide string-constant string-constants
         string-constant-in-current-language?
         this-language all-languages set-language-pref)
(provide
 (contract-out
  [string-constant-language? (-> any/c boolean?)]
  [call-with-current-language (-> string-constant-language? (-> any) any)]
  [string-constant? (-> any/c boolean?)]
  [dynamic-string-constant (-> string-constant? string?)]
  [dynamic-string-constants (-> string-constant? (listof string?))]
  [dynamic-string-constant-in-current-language? (-> string-constant? boolean?)]))

;; set-language-pref : symbol -> void
(define (set-language-pref language)
  (put-preferences (list 'plt:human-language) (list language)))

;; table : (listof (list symbol regexp regexp mod-path))
;; this table indicates what the default value of the natural language
;; preference is. the first regexp is used under Windows and the second
;; is used on other platforms. All regexps are compared to the result
;; of (system-language+country)
(define get-table
  (let ([table #f])
    (λ ()
      (unless table
        (define get-info-key 'string-constants-info)
        (with-handlers ([exn:fail?
                         (λ (x) (set! table 'failed))])
          (set! table
                (apply
                 append
                 (filter
                  values
                  (for/list ([dir (in-list (find-relevant-directories (list get-info-key)))])
                    (define get-info (get-info/full dir))
                    (cond
                      [get-info
                       (define info (get-info get-info-key))
                       (define valid-info-entry?
                         (listof (list/c symbol? regexp? regexp? (and/c module-path? (not/c path-string?)))))
                       (cond
                         [(valid-info-entry? info)
                          info]
                         [else #f])])))))))
      (cond
        [(equal? table 'failed) '()]
        [else table]))))

;; default-language : -> symbol
;; uses `table' and system-language+contry to find what language to start with
(define (default-language)
  (let ([slc (system-language+country)])
    (let loop ([table (get-table)])
      (if (null? table)
        'english
        (let ([ent (car table)])
          (if (or (regexp-match (cadr ent) slc)
                  (and (cddr ent)
                       (regexp-match (caddr ent) slc)))
            (car ent)
            (loop (cdr table))))))))

(define-struct sc (language-name constants [ht #:mutable]))

;; english first, then alphabetically
(define (sc<? a b)
  (cond
    [(equal? (sc-language-name a) 'english) #t]
    [(equal? (sc-language-name b) 'english) #f]
    [else (symbol<? (sc-language-name a) (sc-language-name b))]))

(define-logger string-constants)
(define built-in-string-constant-sets
  (list
   (make-sc 'english english:string-constants #f)
   (make-sc 'german german:string-constants #f)
   (make-sc 'french french:string-constants #f)
   (make-sc 'danish danish:string-constants #f)
   (make-sc 'portuguese portuguese:string-constants #f)
   (make-sc 'japanese japanese:string-constants #f)
   (make-sc 'traditional-chinese traditional-chinese:string-constants #f)
   (make-sc 'simplified-chinese simplified-chinese:string-constants #f)
   (make-sc 'bulgarian bulgarian:string-constants #f)
   (make-sc 'russian russian:string-constants #f)
   (make-sc 'ukrainian ukrainian:string-constants #f)))

(define (get-available-string-constant-sets)
  (sort
   (append
    built-in-string-constant-sets
    (filter
     values
     (for/list ([table-entry (in-list (get-table))])
       (with-handlers ([exn:fail?
                        (λ (x)
                          (log-string-constants-error
                           "failed to load ~a string-constants from ~s:\n~a"
                           (list-ref table-entry 0)
                           (list-ref table-entry 3)
                           (let ([sp (open-output-string)])
                             (parameterize ([current-error-port sp])
                               ((error-display-handler)
                                (exn-message x)
                                x))
                             (get-output-string sp)))
                          #f)])
         (make-sc (list-ref table-entry 0)
                  (dynamic-require (list-ref table-entry 3) 'string-constants)
                  #f)))))
   sc<?))

(define english-string-constant-set (car built-in-string-constant-sets))

;; language : symbol
(define language
  (or (let ([env-lang (getenv "PLTSTRINGCONSTANTSLANG")])
        (and (string? env-lang)
             (string->symbol env-lang)))
      (with-handlers ([exn:fail? (lambda (_) (default-language))])
        (get-preference 'plt:human-language (lambda () (default-language))))))

(define (language-sc language)
  (for/or ([sc (in-list (get-available-string-constant-sets))])
    (and (equal? language (sc-language-name sc))
         sc)))

(define (string-constant-language? x) (and (language-sc x) #t))

(define the-sc (make-parameter (or (language-sc language)
                                   english-string-constant-set)))

(define (call-with-current-language language thunk)
  (parameterize ((the-sc (language-sc language)))
    (thunk)))

(define (dynamic-string-constant key) 
  (dynamic-string-constant/who (the-sc) key 'dynamic-string-constant))

(define (dynamic-string-constants key)
  (for/list ([sc (in-list (get-available-string-constant-sets))])
    (dynamic-string-constant/who sc key 'dynamic-string-constants)))

(define (dynamic-string-constant/who an-sc key who)
  (show-warning-message)
  (hash-ref (sc-constants an-sc) key
            (λ ()
              (hash-ref (sc-constants english-string-constant-set)
                        key
                        (λ ()
                          (error who
                                 "unknown string-constant\n  key: ~e" key))))))
(define (dynamic-string-constant-in-current-language? key)
  (hash-has-key? (sc-constants (the-sc)) key))

(define (string-constant? sym)
  (and (hash-ref (sc-constants english-string-constant-set) sym #f)
       #t))
              

(define already-warned? #f)
(define (show-warning-message)
  (when env-var-set
    (unless already-warned?
      (set! already-warned? #t)
      ;; type no-warning-cache-key = (cons symbol symbol)
      ;; warning-table : (listof (list no-warning-cache-key (listof (list sym string))))
      (define warning-table null)
      (define (extract-ht sc)
        (unless (sc-ht sc)
          (define ht (make-hash))
          (for ([(ent val) (in-hash (sc-constants sc))])
            (hash-set! ht ent #t))
          (set-sc-ht! sc ht))
        (sc-ht sc))
      (define (check-one-way sc1 sc2)
        (define assoc1 (sc-constants sc1))
        (define assoc2 (sc-constants sc2))
        (define ht2 (extract-ht sc2))
        (for ([(constant1 value1) (in-hash assoc1)])
          (define pair2 (hash-ref ht2 constant1 #f))
          (unless pair2
            (define no-warning-cache-key (cons (sc-language-name sc1)
                                               (sc-language-name sc2)))
            (when (or (env-var-set? (sc-language-name sc1))
                      (env-var-set? (sc-language-name sc2)))
              (cond
                [(memf (lambda (ent) (equal? (mcar ent) no-warning-cache-key)) 
                       warning-table)
                 =>
                 (lambda (x)
                   (let ([ent (car x)])
                     (set-mcdr! ent (cons (list constant1 value1) (mcdr ent)))))]
                [else
                 (set! warning-table (cons (mcons no-warning-cache-key
                                                  (list (list constant1 value1)))
                                           warning-table))])))))
      
      (for ([x (in-list (cdr (get-available-string-constant-sets)))])
        (check-one-way x english-string-constant-set)
        (check-one-way english-string-constant-set x))
      
      (define sp (open-output-string))
      (for ([bad (in-list warning-table)])
        (define lang-pair (mcar bad))
        (define constants (mcdr bad))
        (define lang1-name (car lang-pair))
        (define lang2-name (cdr lang-pair))
        (fprintf sp "WARNING: language ~a has but ~a does not:\n"
                 lang1-name
                 lang2-name)
        (define sorted-constants
          (sort constants string<?
                #:key (λ (p) (symbol->string (car p)))
                #:cache-keys? #t))
        (for ([x (in-list sorted-constants)])
          (fprintf sp "   ~s\n" x))
        (newline sp))
      
      (with-handlers ([exn:fail? (lambda (x) (void))])
        ;; the output port may no longer be there, in which case
        ;; we just give up on printing
        (eprintf "~a" (get-output-string sp))))))

;; env-var-set? : symbol -> boolean
;; returns #t if the user has requested this langage info.
;; If the environment variable is set to something that
;; isn't well-formed according to `read' you get all output
;; If the environment variable is set to a symbol (according to read)
;; you get that language. If it is set to a list of symbols
;; (again, according to read) you get those languages.
;; if it is set to anything else, you get all languages.
(define (env-var-set? lang)
  (cond [(symbol? specific) (equal? lang specific)]
        [(list? specific) (member lang specific)]
        [else #t]))

(define env-var-set
  (or (getenv "PLTSTRINGCONSTANTS")
      (getenv "STRINGCONSTANTS")))

(define specific
  (and env-var-set
       (with-handlers ([exn:fail:read? (lambda (x) #t)])
         (read (open-input-string env-var-set)))))


(define-for-syntax (check-name name-stx stx)
  (define datum (syntax->datum name-stx))
  (unless (symbol? datum)
    (raise-syntax-error #f (format "expected name, got: ~s" datum) stx))
  (define default-val (hash-ref english-ct:string-constants datum #f))
  (unless default-val
    (raise-syntax-error
     #f
     (format "~a is not a known string constant" datum)
     stx)))

(define-syntax (string-constant stx)
  (syntax-case stx ()
    [(_ name)
     (begin
       (check-name #'name stx)
       #'(dynamic-string-constant 'name))]))

(define-syntax (string-constants stx)
  (syntax-case stx ()
    [(_ name)
     (begin
       (check-name #'name stx)
       #'(dynamic-string-constants 'name))]))

(define-syntax (string-constant-in-current-language? stx)
  (syntax-case stx ()
    [(_ name)
     (check-name #'name stx)
     #'(dynamic-string-constant-in-current-language? 'name)]))

(define (this-language) language)

(define (all-languages) (map sc-language-name (get-available-string-constant-sets)))
