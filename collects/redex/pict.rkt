#lang racket/base

(require racket/contract
         "private/pict.rkt"
         "private/core-layout.rkt"
         "private/loc-wrapper.rkt"
         "reduction-semantics.rkt"
         texpict/mrpict)

(define reduction-rule-style/c
  (symbols 'compact-vertical
           'vertical 
           'vertical-overlapping-side-conditions
           'horizontal-left-align
           'horizontal))

(provide reduction-rule-style/c render-term term->pict)

(provide/contract
 [render-reduction-relation
  (->i ([rel reduction-relation?])
       ([file (or/c false/c path-string?)]
        #:style
        [style reduction-rule-style/c])
       [result (file)
               (if (path-string? file)
                   void?
                   pict?)])]
 [reduction-relation->pict (->* (reduction-relation?)
                                (#:style reduction-rule-style/c)
                                pict?)]
 [render-reduction-relation-rules (parameter/c (or/c false/c (listof (or/c symbol? string? exact-nonnegative-integer?))))]
 
 [language->pict (->* (compiled-lang?)
                      (#:nts (or/c false/c (listof (or/c string? symbol?))))
                      pict?)]
 [render-language
  (->i ([lang compiled-lang?])
       ([file (or/c false/c path-string?)]
        #:nts
        [nts (or/c false/c (listof (or/c string? symbol?)))])
       [result (file)
               (if (path-string? file)
                   void?
                   pict?)])])

; syntax
(provide relation->pict
         metafunction->pict
         metafunctions->pict
         judgment-form->pict
         
         render-relation
         render-metafunction
         render-metafunctions
         render-judgment-form)

(provide/contract
 [render-language-nts (parameter/c (or/c false/c (listof (or/c string? symbol?))))]
 [extend-language-show-union (parameter/c boolean?)]
 [current-text (parameter/c (-> string? text-style/c number? pict?))])

(provide/contract
 [label-style (parameter/c text-style/c)]
 [literal-style (parameter/c text-style/c)]
 [grammar-style (parameter/c text-style/c)]
 [paren-style (parameter/c text-style/c)]
 [metafunction-style (parameter/c text-style/c)]
 [default-style (parameter/c text-style/c)]
 [non-terminal-style (parameter/c text-style/c)]
 [non-terminal-subscript-style (parameter/c text-style/c)]
 [non-terminal-superscript-style (parameter/c text-style/c)]
 [linebreaks (parameter/c (or/c false/c (listof boolean?)))]
 [curly-quotes-for-strings (parameter/c boolean?)]
 [white-bracket-sizing (parameter/c
                        (-> string? number? (values number? number? number? number?)))]
 [horizontal-bar-spacing (parameter/c exact-nonnegative-integer?)]
 [relation-clauses-combine (parameter/c (-> (listof pict?) pict?))])

(provide/contract
 [rule-pict-style 
  (parameter/c reduction-rule-style/c)]
 [arrow-space (parameter/c natural-number/c)]
 [label-space (parameter/c natural-number/c)]
 [metafunction-cases (parameter/c (or/c #f (and/c pair? (listof (and/c integer? (or/c zero? positive?))))))]
 [metafunction-pict-style 
  (parameter/c (symbols 'left-right
                        'left-right/vertical-side-conditions
                        'left-right/compact-side-conditions
                        'left-right/beside-side-conditions
                        'up-down
                        'up-down/vertical-side-conditions
                        'up-down/compact-side-conditions))]
 [delimit-ellipsis-arguments? (parameter/c any/c)])

(provide/contract
 [label-font-size (parameter/c (and/c (between/c 1 255) integer?))]
 [default-font-size (parameter/c (and/c (between/c 1 255) integer?))]
 [metafunction-font-size (parameter/c (and/c (between/c 1 255) integer?))]
 [reduction-relation-rule-separation (parameter/c (and/c integer? positive? exact?))])

(provide
 build-lw
 lw
 lw?
 lw-e
 lw-line
 lw-line-span
 lw-column
 lw-column-span)

(provide to-lw
         to-lw/stx
         (struct-out lw))

(require (prefix-in lw/ct: "private/loc-wrapper-ct.rkt")
         (prefix-in lw/rt: "private/loc-wrapper-rt.rkt"))
(define (to-lw/stx stx)
  (let loop ([stx (lw/ct:to-lw/proc stx)])
    (syntax-case stx (init-loc-wrapper make-lw add-spans list quote)
      [(make-lw arg ...) 
       (apply make-lw (map loop (syntax->list #'(arg ...))))]
      [(init-loc-wrapper arg ...)
       (apply lw/rt:init-loc-wrapper (map loop (syntax->list #'(arg ...))))]
      [(add-spans arg ...)
       (apply lw/rt:add-spans (map loop (syntax->list #'(arg ...))))]
      [(list arg ...)
       (apply list (map loop (syntax->list #'(arg ...))))]
      [(quote arg)
       (syntax->datum #'arg)]
      [_ 
       (let ([x (syntax-e stx)])
         (unless (or (number? x)
                     (string? x)
                     (boolean? x))
           (error 'to-lw/stx "unk thing: ~s\n" (syntax->datum stx)))
         x)])))

(provide/contract
 [just-before (-> (or/c pict? string? symbol?) lw? lw?)]
 [just-after (-> (or/c pict? string? symbol?) lw? lw?)])
(provide with-unquote-rewriter
         with-compound-rewriter
         with-compound-rewriters
         with-atomic-rewriter)

(provide/contract
 [set-arrow-pict! (-> symbol? (-> pict?) void?)]
 [arrow->pict (-> symbol? pict?)]
 
 [lw->pict
  (-> (or/c (listof symbol?) compiled-lang?) lw? pict?)]
 [render-lw
  (-> (or/c (listof symbol?) compiled-lang?) lw? pict?)])
