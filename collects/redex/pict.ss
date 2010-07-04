#lang scheme/base

(require scheme/contract
         "private/pict.ss"
         "private/core-layout.ss"
         "private/loc-wrapper.ss"
         "reduction-semantics.ss"
         (lib "mred.ss" "mred")
         (lib "mrpict.ss" "texpict"))

(define reduction-rule-style/c
  (symbols 'compact-vertical
           'vertical 
           'vertical-overlapping-side-conditions
           'horizontal))

(provide reduction-rule-style/c)

(provide/contract 
 [render-reduction-relation
  (->d ([rel reduction-relation?])
       ([file (or/c false/c path-string?)]
        #:style [style reduction-rule-style/c])
       [result (if (path-string? file)
                   void?
                   pict?)])]
 [reduction-relation->pict (->* (reduction-relation?)
                                (#:style reduction-rule-style/c)
                                pict?)]
 [render-reduction-relation-rules (parameter/c (or/c false/c (listof (or/c symbol? string?))))]
 
 [language->pict (->* (compiled-lang?)
                      (#:nts (or/c false/c (listof (or/c string? symbol?))))
                      pict?)]
 [render-language
  (->d ([lang compiled-lang?])
       ([file (or/c false/c path-string?)]
        #:nts [nts (or/c false/c (listof (or/c string? symbol?)))])
       [result (if (path-string? file)
                   void?
                   pict?)])])

; syntax
(provide metafunction->pict
         render-metafunction)

(provide/contract
 [render-language-nts (parameter/c (or/c false/c (listof (or/c string? symbol?))))]
 [extend-language-show-union (parameter/c boolean?)]
 [current-text (parameter/c (-> string? text-style/c number? pict?))])

(provide/contract
 [label-style (parameter/c text-style/c)]
 [literal-style (parameter/c text-style/c)]
 [metafunction-style (parameter/c text-style/c)]
 [default-style (parameter/c text-style/c)]
 [non-terminal-style (parameter/c text-style/c)]
 [non-terminal-subscript-style (parameter/c text-style/c)]
 [linebreaks (parameter/c (or/c false/c (listof boolean?)))]
 [curly-quotes-for-strings (parameter/c boolean?)]
 [white-bracket-sizing (parameter/c
                        (-> string? number? (values number? number? number? number?)))])

(provide/contract
 [rule-pict-style 
  (parameter/c reduction-rule-style/c)]
 [arrow-space (parameter/c natural-number/c)]
 [label-space (parameter/c natural-number/c)]
 [metafunction-pict-style 
  (parameter/c (symbols 'left-right
                        'up-down))])

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
         (struct-out lw))

(provide/contract
 [just-before (-> (or/c pict? string? symbol?) lw? lw?)]
 [just-after (-> (or/c pict? string? symbol?) lw? lw?)])
(provide with-unquote-rewriter
         with-compound-rewriter
         with-atomic-rewriter)

(provide/contract
 [set-arrow-pict! (-> symbol? (-> pict?) void?)]
 
 [lw->pict
  (-> (or/c (listof symbol?) compiled-lang?) lw? pict?)]
 [render-lw
  (-> (or/c (listof symbol?) compiled-lang?) lw? pict?)])