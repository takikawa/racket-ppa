#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../base.ss"
         (only-in "../basic.ss" aux-elem itemize)
         "../scheme.ss"
         (only-in "../core.ss" make-style plain)
         "manual-utils.ss"
         scheme/list
         scheme/contract
         scheme/string)

(provide (rename-out [hyperlink link])
         (rename-out [other-doc other-manual])
         (rename-out [centered centerline])
         image
         (rename-out [image image/plain])
         itemize
         aux-elem)

(define styling-f/c
  (() () #:rest (listof pre-content?) . ->* . element?))
(define-syntax-rule (provide-styling id ...)
  (provide/contract [id styling-f/c] ...))
(provide-styling schememodfont schemeoutput
                 schemeerror schemefont schemevalfont schemeresultfont schemeidfont schemevarfont
                 schemeparenfont schemekeywordfont schememetafont
                 onscreen defterm filepath exec envvar Flag DFlag PFlag DPFlag math
                 procedure
                 indexed-file indexed-envvar idefterm pidefterm)
(provide/contract
 [PLaneT element?]
 [void-const element?]
 [undefined-const element?]
 [hash-lang (-> element?)]
 [etc string?]
 [inset-flow (() () #:rest (listof pre-content?) . ->* . any/c)] ; XXX no docs and bad return contract
 [litchar (() () #:rest (listof string?) . ->* . element?)]
 [t (() () #:rest (listof pre-content?) . ->* . paragraph?)]
 [commandline (() () #:rest (listof pre-content?) . ->* . paragraph?)]
 [menuitem (string? string? . -> . element?)]) 

(define PLaneT (make-element "planetName" '("PLaneT")))

(define etc "etc.") ; so we can fix the latex space, one day

(define (litchar . strs)
  (let ([s (string-append* (map (lambda (s) (regexp-replace* "\n" s " "))
                                strs))])
    (if (regexp-match? #rx"^ *$" s)
      (make-element input-background-color (list (hspace (string-length s))))
      (let ([^spaces (car (regexp-match-positions #rx"^ *" s))]
            [$spaces (car (regexp-match-positions #rx" *$" s))])
        (make-element
         input-background-color
         (list (hspace (cdr ^spaces))
               (make-element input-color
                             (list (substring s (cdr ^spaces) (car $spaces))))
               (hspace (- (cdr $spaces) (car $spaces)))))))))

(define (onscreen . str)
  (make-element 'sf (decode-content str)))
(define (menuitem menu item)
  (make-element 'sf (list menu "|" item)))
(define (defterm . str)
  (make-element 'italic (decode-content str)))
(define (idefterm . str)
  (let ([c (decode-content str)])
    (make-element 'italic c)))
(define (schemefont . str)
  (apply tt str))
(define (schemevalfont . str)
  (make-element value-color (decode-content str)))
(define (schemeresultfont . str)
  (make-element result-color (decode-content str)))
(define (schemeidfont . str)
  (make-element symbol-color (decode-content str)))
(define (schemevarfont . str)
  (make-element variable-color (decode-content str)))
(define (schemeparenfont . str)
  (make-element paren-color (decode-content str)))
(define (schememetafont . str)
  (make-element meta-color (decode-content str)))
(define (schememodfont . str)
  (make-element module-color (decode-content str)))
(define (schemekeywordfont . str)
  (make-element keyword-color (decode-content str)))
(define (filepath . str)
  (make-element 'tt (append (list "\"") (decode-content str) (list "\""))))
(define (indexed-file . str)
  (let* ([f (apply filepath str)]
         [s (element->string f)])
    (index* (list (clean-up-index-string
                   (substring s 1 (sub1 (string-length s)))))
            (list f)
            f)))
(define (exec . str)
  (if (andmap string? str)
    (make-element 'tt str)
    (make-element #f (map (lambda (s)
                            (if (string? s)
                              (make-element 'tt (list s))
                              s))
                          str))))
(define (Flag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "-" (decode-content str))))))
(define (DFlag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "--" (decode-content str))))))
(define (PFlag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "+" (decode-content str))))))
(define (DPFlag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "++" (decode-content str))))))
(define (envvar . str)
  (make-element 'tt (decode-content str)))
(define (indexed-envvar . str)
  (let* ([f (apply envvar str)]
         [s (element->string f)])
    (index* (list s) (list f) f)))
(define (procedure . str)
  (make-element result-color `("#<procedure:" ,@(decode-content str) ">")))

(define (schemeoutput . str)
  (make-element output-color (decode-content str)))
(define (schemeerror . str)
  (make-element error-color (decode-content str)))

(define (t . str)
  (decode-paragraph str))

(define (inset-flow . c)
  (make-blockquote "insetpara" (flow-paragraphs (decode-flow c))))

(define (commandline . s)
  (make-paragraph (cons (hspace 2) (map (lambda (s)
                                          (if (string? s)
                                            (make-element 'tt (list s))
                                            s))
                                        s))))

(define (pidefterm . s)
  (let ([c (apply defterm s)])
    (index (string-append (content->string (element-content c)) "s")
           c)))

(define (hash-lang)
  (make-link-element
   module-link-color
   (list (schememodfont "#lang"))
   `(part ,(doc-prefix '(lib "scribblings/guide/guide.scrbl") "hash-lang"))))

(define void-const
  (schemeresultfont "#<void>"))
(define undefined-const
  (schemeresultfont "#<undefined>"))

(define (link url 
              #:underline? [underline? #t]
              #:style [style (if underline? #f "plainlink")]
              . str)
  (apply hyperlink url #:style (if style (make-style style null) plain) str))

(define (math . s)
  (let ([c (decode-content s)])
    (make-element
     #f
     (append-map
      (lambda (i)
        (let loop ([i i])
          (cond
            [(string? i)
             (cond
               [(regexp-match #px"^(.*)_([a-zA-Z0-9]+)(.*)$" i)
                => (lambda (m)
                     (append (loop (cadr m))
                             (list (make-element 'subscript
                                                 (loop (caddr m))))
                             (loop (cadddr m))))]
               [(regexp-match #px"^(.*)\\^([a-zA-Z0-9]+)(.*)$" i)
                => (lambda (m)
                     (append (loop (cadr m))
                             (list (make-element 'superscript
                                                 (loop (caddr m))))
                             (loop (cadddr m))))]
               [(regexp-match #px"^(.*)([()0-9{}\\[\\]\u03C0])(.*)$" i)
                => (lambda (m)
                     (append (loop (cadr m))
                             (list (caddr m))
                             (loop (cadddr m))))]
               [else
                (list (make-element 'italic (list i)))])]
            [(eq? i 'rsquo) (list 'prime)]
            [else (list i)])))
      c))))