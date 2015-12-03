#lang racket/base

(require racket/contract
         "private/pict.rkt"
         "private/core-layout.rkt"
         redex/private/struct
         redex/private/loc-wrapper
         redex/reduction-semantics
         texpict/mrpict)

(define reduction-rule-style/c
  (or/c 'vertical 
        'compact-vertical
        'vertical-overlapping-side-conditions
        'horizontal
        'horizontal-left-align
        'horizontal-side-conditions-same-line
        (-> (listof rule-pict-info?) pict?)))

(provide reduction-rule-style/c render-term term->pict
         term->pict/pretty-write
         render-term/pretty-write)

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
 [render-reduction-relation-rules 
  (parameter/c (or/c #f (listof (or/c symbol? string? exact-nonnegative-integer?))))]
 
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
                   pict?)])]
 
 [rule-pict-info-arrow (-> rule-pict-info? symbol?)]
 [rule-pict-info-lhs (-> rule-pict-info? pict?)]
 [rule-pict-info-rhs (-> rule-pict-info? pict?)]
 [rule-pict-info-label (-> rule-pict-info? (or/c symbol? #f))]
 [rule-pict-info-computed-label (-> rule-pict-info? (or/c pict? #f))]
 [rule-pict-info->side-condition-pict (->* (rule-pict-info?) ((and/c positive? real?)) pict?)]
 [rule-pict-info? (-> any/c boolean?)])

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
 [extend-language-show-extended-order (parameter/c boolean?)]
 [current-text (parameter/c (-> string? text-style/c number? pict?))])

(provide/contract
 [non-terminal-gap-space (parameter/c real?)]
 [metafunction-gap-space (parameter/c real?)]
 [metafunction-rule-gap-space (parameter/c real?)]
 [metafunction-line-gap-space (parameter/c real?)]
 [metafunction-combine-contract-and-rules (parameter/c (pict? pict? . -> . pict?))]
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
 [relation-clauses-combine (parameter/c (-> (listof pict?) pict?))]
 [where-make-prefix-pict (parameter/c (-> pict?))]
 [where-combine (parameter/c (-> pict? pict? pict?))])

(provide/contract
 [rule-pict-style 
  (parameter/c reduction-rule-style/c)]
 [arrow-space (parameter/c natural-number/c)]
 [label-space (parameter/c natural-number/c)]
 [metafunction-cases (parameter/c (or/c #f (listof (or/c exact-nonnegative-integer? string?))))]
 [judgment-form-cases (parameter/c (or/c #f (and/c (listof (or/c exact-nonnegative-integer? string?))
                                                   pair?)))]
 [metafunction-pict-style 
  (parameter/c (symbols 'left-right
                        'left-right/vertical-side-conditions
                        'left-right/compact-side-conditions
                        'left-right/beside-side-conditions
                        'up-down
                        'up-down/vertical-side-conditions
                        'up-down/compact-side-conditions))]
 [metafunction-up/down-indent (parameter/c (>=/c 0))]
 
 [delimit-ellipsis-arguments? (parameter/c any/c)]
 
 [default-white-square-bracket (-> boolean? pict?)]
 [homemade-white-square-bracket (-> boolean? pict?)]
 [white-square-bracket (parameter/c (-> boolean? pict?))])

(provide/contract
 [label-font-size (parameter/c (and/c (between/c 1 255) integer?))]
 [default-font-size (parameter/c (and/c (between/c 1 255) integer?))]
 [metafunction-font-size (parameter/c (and/c (between/c 1 255) integer?))]
 [reduction-relation-rule-separation (parameter/c real?)]
 [reduction-relation-rule-extra-separation (parameter/c real?)]
 [reduction-relation-rule-line-separation (parameter/c real?)]

 [current-render-pict-adjust (parameter/c (pict? symbol? . -> . pict?))])

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
