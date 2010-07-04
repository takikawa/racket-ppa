#lang scheme/base
(require (lib "mrpict.ss" "texpict")
         (lib "utils.ss" "texpict")
         scheme/gui/base
         scheme/class
         scheme/match
         (only-in scheme/list drop-right last partition)
         "reduction-semantics.ss"
         "struct.ss"
         "loc-wrapper.ss"
         "matcher.ss"
         "arrow.ss"
         "core-layout.ss")
(require (for-syntax scheme/base))

(provide render-term 
         term->pict
         
         language->pict
         render-language
         render-language-nts
         
         reduction-relation->pict
         render-reduction-relation
         render-reduction-relation-rules
         
         metafunction->pict
         metafunctions->pict
         render-metafunction
         render-metafunctions
         
         basic-text
         
         default-style
         label-style
         literal-style
         metafunction-style
         
         label-font-size
         default-font-size
         metafunction-font-size
         reduction-relation-rule-separation
         
         linebreaks
         
         just-before
         just-after         
         
         rule-pict-style
         arrow-space
         label-space
         metafunction-pict-style
         metafunction-cases
         compact-vertical-min-width
         extend-language-show-union
         set-arrow-pict!)


;                                                                             
;                                                                             
;                                                                             
;                       ;;;;                       ;    ;;                    
;                       ;;;;                      ;;    ;;                    
;  ;;; ;;;   ;;;     ;;;;;;; ;;;; ;;;;   ;;;;;  ;;;;;        ;;;;   ;;;; ;;;  
;  ;;;;;;;  ;;;;;   ;;;;;;;; ;;;; ;;;;  ;;;;;; ;;;;;; ;;;;  ;;;;;;  ;;;;;;;;; 
;  ;;;; ;; ;;;; ;; ;;;;;;;;; ;;;; ;;;; ;;;;;;;  ;;;;  ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;;    ;;;;;;; ;;;; ;;;; ;;;; ;;;; ;;;;     ;;;;  ;;;; ;;;; ;;; ;;;; ;;;; 
;  ;;;;    ;;;;;   ;;;;;;;;; ;;;; ;;;; ;;;;;;;  ;;;;; ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;;     ;;;;;;  ;;;;;;;; ;;;;;;;;;  ;;;;;;  ;;;;; ;;;;  ;;;;;;  ;;;; ;;;; 
;  ;;;;      ;;;;    ;;;;;;;  ;;; ;;;;   ;;;;;   ;;;; ;;;;   ;;;;   ;;;; ;;;; 
;                                                                             
;                                                                             
;                                                                             
;                                                               
;                                                               
;                                                               
;                  ;;;;              ;    ;;                    
;                  ;;;;             ;;    ;;                    
;  ;;; ;;;   ;;;   ;;;; ;;;;;;;   ;;;;;        ;;;;   ;;;; ;;;  
;  ;;;;;;;  ;;;;;  ;;;; ;;;;;;;; ;;;;;; ;;;;  ;;;;;;  ;;;;;;;;; 
;  ;;;; ;; ;;;; ;; ;;;;     ;;;;  ;;;;  ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;;    ;;;;;;; ;;;;  ;;;;;;;  ;;;;  ;;;; ;;;; ;;; ;;;; ;;;; 
;  ;;;;    ;;;;;   ;;;; ;;  ;;;;  ;;;;; ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;;     ;;;;;; ;;;; ;;;;;;;;  ;;;;; ;;;;  ;;;;;;  ;;;; ;;;; 
;  ;;;;      ;;;;  ;;;;  ;; ;;;;   ;;;; ;;;;   ;;;;   ;;;; ;;;; 
;                                                               
;                                                               
;                                                               

(define (do-reduction-relation->pict what rr style)
  (let ([rules (render-reduction-relation-rules)])
    ((rule-pict-style->proc style)
     (map (rr-lws->trees (language-nts (reduction-relation-lang rr)))
          (if rules
              (let ([ht (make-hash)])
                (for-each (lambda (rp)
                            (hash-set! ht (rule-pict-label rp) rp))
                          (reduction-relation-lws rr))
                (map (lambda (label)
                       (hash-ref ht label
                                 (lambda ()
                                   (error what
                                          "no rule found for label: ~e"
                                          label))))
                     rules))
              (reduction-relation-lws rr))))))

(define (reduction-relation->pict rr #:style [style (rule-pict-style)]) 
  (do-reduction-relation->pict 'reduction-relation->pict rr style))

(define render-reduction-relation-rules (make-parameter #f))

(define (render-reduction-relation rr [filename #f]
                                   #:style [style (rule-pict-style)])
  (if filename
      (save-as-ps (λ () (do-reduction-relation->pict 'render-reduction-relation rr style))
                  filename)
      (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
        (do-reduction-relation->pict 'render-reduction-relation rr style))))

(define ((rr-lws->trees nts) rp)
  (let ([tp (λ (x) (lw->pict nts x))])
    (make-rule-pict (rule-pict-arrow rp)
                    (tp (rule-pict-lhs rp))
                    (tp (rule-pict-rhs rp))
                    (rule-pict-label rp)
                    (map (lambda (v) 
                           (if (pair? v)
                               (cons (tp (car v)) (tp (cdr v)))
                               (tp v)))
                         (rule-pict-side-conditions/pattern-binds rp))
                    (map tp (rule-pict-fresh-vars rp)))))

(define current-label-extra-space (make-parameter 0))
(define reduction-relation-rule-separation (make-parameter 4))

(define ((rule-picts->pict/horizontal left-column-align) rps)
  (let* ([sep 2]
         [max-rhs (apply max
                         0
                         (map pict-width
                              (map rule-pict-rhs rps)))]
         [max-w (apply max
                       0
                       (map (lambda (rp)
                              (+ sep sep
                                 (pict-width (rule-pict-lhs rp))
                                 (pict-width (arrow->pict (rule-pict-arrow rp)))
                                 (pict-width (rule-pict-rhs rp))))
                            rps))])
    (table 4
           (apply
            append
            (map (lambda (rp)
                   (let ([arrow (hbl-append (blank (arrow-space) 0)
                                            (arrow->pict (rule-pict-arrow rp))
                                            (blank (arrow-space) 0))]
                         [lhs (rule-pict-lhs rp)]
                         [rhs (rule-pict-rhs rp)]
                         [spc (basic-text " " (default-style))]
                         [label (hbl-append (blank (label-space) 0) (rp->pict-label rp))]
                         [sep (blank 4)])
                     (list lhs arrow rhs label
                           (blank) (blank)
                           (let ([sc (rp->side-condition-pict rp max-w)])
                             (inset sc (min 0 (- max-rhs (pict-width sc))) 0 0 0))
                           (blank)
                           sep (blank) (blank) (blank))))
                 rps))
           (list* left-column-align ctl-superimpose ltl-superimpose)
           (list* left-column-align ctl-superimpose ltl-superimpose)
           (list* sep sep (+ sep (current-label-extra-space))) 2)))

(define arrow-space (make-parameter 0))
(define label-space (make-parameter 0))

(define ((make-vertical-style side-condition-combiner) rps)
  (let* ([mk-top-line-spacer
          (λ (rp)
            (hbl-append (rule-pict-lhs rp)
                        (basic-text " " (default-style))
                        (arrow->pict (rule-pict-arrow rp))
                        (basic-text " " (default-style))
                        (rp->pict-label rp)))]
         [mk-bot-line-spacer
          (λ (rp)
            (rt-superimpose
             (rule-pict-rhs rp)
             (rp->side-condition-pict rp +inf.0)))]
         [multi-line-spacer
          (ghost
           (launder
            (ctl-superimpose 
             (apply ctl-superimpose (map mk-top-line-spacer rps))
             (apply ctl-superimpose (map mk-bot-line-spacer rps)))))]
         [spacer (dc void 
                     (pict-width multi-line-spacer)
                     (pict-descent multi-line-spacer) ;; probably could be zero ...
                     0
                     (pict-descent multi-line-spacer))])
    (apply
     vl-append
     (add-between
      (blank 0 (reduction-relation-rule-separation))
      (map (λ (rp)
             (side-condition-combiner
              (vl-append
               (ltl-superimpose 
                (htl-append (rule-pict-lhs rp)
                            (basic-text " " (default-style))
                            (arrow->pict (rule-pict-arrow rp)))
                (rtl-superimpose 
                 spacer
                 (rp->pict-label rp)))
               (rule-pict-rhs rp))
              (rp->side-condition-pict rp +inf.0)))
           rps)))))

(define compact-vertical-min-width (make-parameter 0))

(define rule-picts->pict/vertical 
  (make-vertical-style vr-append))

(define rule-picts->pict/vertical-overlapping-side-conditions
  (make-vertical-style rbl-superimpose))

(define (rule-picts->pict/compact-vertical rps)
  (let* ([max-w (apply max
                       (compact-vertical-min-width)
                       (map pict-width
                            (append
                             (map rule-pict-lhs rps)
                             (map rule-pict-rhs rps))))]
         [scs (map (lambda (rp)
                     (rp->side-condition-pict rp max-w))
                   rps)]
         [labels (map (lambda (rp)
                        (hbl-append (blank (label-space) 0) (rp->pict-label rp)))
                      rps)]
         [total-w (apply max
                         max-w
                         (append (map pict-width scs)
                                 (map (lambda (lbl)
                                        (+ max-w 2 (label-space) (pict-width lbl)))
                                      labels)))]
         [one-line
          (lambda (sep?)
            (lambda (rp sc label)
              (let ([arrow (hbl-append (arrow->pict (rule-pict-arrow rp)) (blank (arrow-space) 0))]
                    [lhs (rule-pict-lhs rp)]
                    [rhs (rule-pict-rhs rp)]
                    [spc (basic-text " " (default-style))]
                    [sep (blank (compact-vertical-min-width)
                                (reduction-relation-rule-separation))]
                    [add-label (lambda (p label)
                                 (htl-append 
                                  p
                                  (inset label (- total-w (pict-width p) (pict-width label))
                                         0 0 0)))])
                (append
                 (if ((apply + (map pict-width (list lhs spc arrow spc rhs)))
                      . < .
                      max-w)
                     (list 
                      (blank) (add-label (hbl-append lhs spc arrow spc rhs) label)
                      (blank) sc)
                     (list (blank) (add-label lhs label)
                           arrow rhs
                           (blank) sc))
                 (if sep? (list (blank) sep) null)))))])
    (if (null? rps)
        (blank)
        (table 2
               (append
                (apply
                 append
                 (map (one-line #t)
                      (drop-right rps 1) 
                      (drop-right scs 1)
                      (drop-right labels 1)))
                ((one-line #f) (last rps) (last scs) (last labels)))
               ltl-superimpose ltl-superimpose
               2 2))))

;; side-condition-pict : (listof pict) (listof (or/c (cons/c pict pict) pict)) number -> pict
;; the elements of pattern-binds/sc that are pairs are bindings (ie "x = <something>")
;;     and the elements of pattern-binds/sc that are just picts are just plain side-conditions
(define (side-condition-pict fresh-vars pattern-binds/sc max-w)
  (let* ([frsh 
          (if (null? fresh-vars)
              null
              (list
               (hbl-append
                (apply 
                 hbl-append
                 (add-between
                  (basic-text ", " (default-style))
                  fresh-vars))
                (basic-text " fresh" (default-style)))))]
         [binds (map (lambda (b)
                       (if (pair? b)
                           (htl-append
                            (car b)
                            (make-=)
                            (cdr b))
                           b))
                     pattern-binds/sc)]
         [lst (add-between
               'comma
               (append
                binds
                frsh))])
    (if (null? lst)
        (blank)
        (let ([where (basic-text " where " (default-style))])
          (let ([max-w (- max-w (pict-width where))])
            (htl-append where
                        (let loop ([p (car lst)][lst (cdr lst)])
                          (cond
                            [(null? lst) p]
                            [(eq? (car lst) 'comma)
                             (loop (htl-append p (basic-text ", " (default-style)))
                                   (cdr lst))]
                            [((+ (pict-width p) (pict-width (car lst))) . > . max-w)
                             (vl-append p
                                        (loop (car lst) (cdr lst)))]
                            [else (loop (htl-append p (car lst)) (cdr lst))]))))))))

(define (rp->side-condition-pict rp max-w)
  (side-condition-pict (rule-pict-fresh-vars rp)
                       (rule-pict-side-conditions/pattern-binds rp)
                       max-w))

(define (rp->pict-label rp)
  (if (rule-pict-label rp)
      (let ([m (regexp-match #rx"^([^_]*)(?:_([^_]*)|)$" 
                             (format "~a" (rule-pict-label rp)))])
        (hbl-append
         ((current-text) " [" (label-style) (label-font-size))
         ((current-text) (cadr m) (label-style) (label-font-size))
         (if (caddr m)
             ((current-text) (caddr m) `(subscript . ,(label-style)) (label-font-size))
             (blank))
         ((current-text) "]" (label-style) (label-font-size))))
      (blank)))

(define (add-between i l)
  (cond
    [(null? l) l]
    [else 
     (cons (car l)
           (apply append 
                  (map (λ (x) (list i x)) (cdr l))))]))

(define (make-horiz-space picts) (blank (pict-width (apply cc-superimpose picts)) 0))

(define rule-pict-style (make-parameter 'vertical))
(define (rule-pict-style->proc style)
  (case style
    [(vertical) rule-picts->pict/vertical]
    [(compact-vertical) rule-picts->pict/compact-vertical]
    [(vertical-overlapping-side-conditions)
     rule-picts->pict/vertical-overlapping-side-conditions]
    [(horizontal-left-align)
     (rule-picts->pict/horizontal ltl-superimpose)]
    [else ;; horizontal
     (rule-picts->pict/horizontal rtl-superimpose)]))

(define (mk-arrow-pict sz style)
  (let ([cache (make-hash)])
    (lambda ()
      (let ([s (default-font-size)])
        ((hash-ref cache s
                   (lambda ()
                     (let ([f (make-arrow-pict sz style 'roman s)])
                       (hash-set! cache s f)
                       f))))))))

(define long-arrow-pict (mk-arrow-pict "xxx" 'straight))
(define short-arrow-pict (mk-arrow-pict "m" 'straight))
(define curvy-arrow-pict (mk-arrow-pict "xxx" 'curvy))
(define short-curvy-arrow-pict (mk-arrow-pict "m" 'curvy))
(define double-arrow-pict (mk-arrow-pict "xxx" 'straight-double))
(define short-double-arrow-pict (mk-arrow-pict "m" 'straight-double))
(define map-arrow-pict (mk-arrow-pict "m" 'map))
(define long-map-arrow-pict (mk-arrow-pict "xxx" 'map))

(define user-arrow-table (make-hasheq))
(define (set-arrow-pict! arr thunk)
  (hash-set! user-arrow-table arr thunk))

(define (arrow->pict arr)
  (let ([ut (hash-ref user-arrow-table arr #f)])
    (if ut
        (ut)
        (case arr
          [(--> -+>) (long-arrow-pict)]
          [(==>) (double-arrow-pict)]
          [(->) (short-arrow-pict)]
          [(=>) (short-double-arrow-pict)]
          [(..>) (basic-text "\u21E2" (default-style))]
          [(>->) (basic-text "\u21a3" (default-style))]
          [(~~>) (curvy-arrow-pict)]
          [(~>) (short-curvy-arrow-pict)]
          [(:->) 
           (if STIX?
               (basic-text "\u21a6" (default-style))
               (map-arrow-pict))]
          [(:-->) 
           (if STIX?
               (basic-text "\u27fc" (default-style))
               (long-map-arrow-pict))]
          [(c->) (basic-text "\u21aa" (default-style))]
          [(-->>) (basic-text "\u21a0" (default-style))]
          [(>--) (basic-text "\u291a" (default-style))]
          [(--<) (basic-text "\u2919" (default-style))]
          [(>>--) (basic-text "\u291c" (default-style))]
          [(--<<) (basic-text "\u291b" (default-style))]
          [else (error 'arrow->pict "unknown arrow ~s" arr)]))))



;                                                                       
;                                                                       
;                                                                       
;  ;;;;                                                                 
;  ;;;;                                                                 
;  ;;;; ;;;;;;;  ;;;; ;;;   ;;;;;;; ;;;; ;;;; ;;;;;;;   ;;;;;;;   ;;;   
;  ;;;; ;;;;;;;; ;;;;;;;;; ;;;;;;;; ;;;; ;;;; ;;;;;;;; ;;;;;;;;  ;;;;;  
;  ;;;;     ;;;; ;;;; ;;;; ;;; ;;;; ;;;; ;;;;     ;;;; ;;; ;;;; ;;;; ;; 
;  ;;;;  ;;;;;;; ;;;; ;;;; ;;;;;;;; ;;;; ;;;;  ;;;;;;; ;;;;;;;; ;;;;;;; 
;  ;;;; ;;  ;;;; ;;;; ;;;;  ;;;;;;; ;;;; ;;;; ;;  ;;;;  ;;;;;;; ;;;;;   
;  ;;;; ;;;;;;;; ;;;; ;;;; ;   ;;;; ;;;;;;;;; ;;;;;;;; ;   ;;;;  ;;;;;; 
;  ;;;;  ;; ;;;; ;;;; ;;;; ;;;;;;;;  ;;; ;;;;  ;; ;;;; ;;;;;;;;   ;;;;  
;                          ;;;;;;;;                    ;;;;;;;;         
;                           ;;;;;;                      ;;;;;;          
;                                                                       

;; type flattened-language-pict-info =
;;   (listof (cons (listof symbol[nt]) (listof loc-wrapper[rhs])))
;; type language-pict-info = 
;;  (union (vector flattened-language-pict-info language-pict-info) 
;;         flattened-language-pict-info)

(define (render-language lang [filename #f] #:nts [nts (render-language-nts)])
  (if filename
      (save-as-ps (λ () (do-language->pict 'render-language lang nts)) filename)
      (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
        (do-language->pict 'render-language lang nts))))

(define (language->pict lang #:nts [nts (render-language-nts)])
  (do-language->pict 'language->pict lang nts))

(define (do-language->pict what lang specd-non-terminals)
  (let ([all-non-terminals (hash-map (compiled-lang-ht lang) (λ (x y) x))])
    (when specd-non-terminals
      (check-non-terminals what specd-non-terminals lang))
    (make-grammar-pict (compiled-lang-pict-builder lang) 
                       (or specd-non-terminals all-non-terminals)
                       all-non-terminals)))

(define render-language-nts (make-parameter #f))

(define (check-non-terminals what nts lang)
  (let ([langs-nts (language-nts lang)])
    (for-each
     (λ (nt) 
       (unless (memq nt langs-nts)
         (error what 
                "the non-terminal ~s is not one of the language's nonterminals (~a)"
                nt
                (if (null? langs-nts)
                    "it has no non-terminals"
                    (apply
                     string-append
                     "which are: "
                     (format "~a" (car langs-nts))
                     (map (λ (x) (format " ~a" x)) (cdr langs-nts)))))))
     nts)))

;; save-as-ps : (-> pict) string -> void
(define (save-as-ps mk-pict filename) 
  (let ([ps-dc (make-ps-dc filename)])
    (parameterize ([dc-for-text-size ps-dc])
      (send ps-dc start-doc "x")
      (send ps-dc start-page)
      (draw-pict (mk-pict) ps-dc 0 0)
      (send ps-dc end-page)
      (send ps-dc end-doc))))

(define (make-ps-dc filename)
  (let ([ps-setup (make-object ps-setup%)])
    (send ps-setup copy-from (current-ps-setup))
    (send ps-setup set-file filename)
    (send ps-setup set-mode 'file)
    (parameterize ([current-ps-setup ps-setup])
      (make-object post-script-dc% #f #f))))

;; raw-info : language-pict-info
;; nts : (listof symbol) -- the nts that the user expects to see
(define (make-grammar-pict raw-info nts all-nts)
  (let* ([info (remove-unwanted-nts nts (flatten-grammar-info raw-info all-nts))]
         [term-space 
          (launder
           (ghost
            (apply cc-superimpose (map (λ (x) (sequence-of-non-terminals (car x)))
                                       info))))])
    (apply vl-append
           (map (λ (line)
                  (htl-append 
                   (rc-superimpose term-space (sequence-of-non-terminals (car line)))
                   (lw->pict
                    all-nts
                    (find-enclosing-loc-wrapper (add-bars-and-::= (cdr line))))))
                info))))

(define (sequence-of-non-terminals nts)
  (let ([draw-nt (lambda (nt)
                   (lw->pict nts (build-lw nt 0 0 0 0)))])
    (let loop ([nts (cdr nts)]
               [pict (draw-nt (car nts))])
      (cond
       [(null? nts) pict]
       [else 
        (loop (cdr nts)
              (hbl-append pict 
                          (non-terminal ", ")
                          (draw-nt (car nts))))]))))


(define extend-language-show-union (make-parameter #f))

;; remove-unwanted-nts : (listof symbol) flattened-language-pict-info -> flattened-language-pict-info
(define (remove-unwanted-nts nts info)
  (filter (λ (x) (not (null? (car x))))
          (map
           (λ (x) (cons (filter (λ (x) (member x nts)) (car x))
                        (cdr x)))
           info)))


;; flatten-grammar-info : language-pict-info (listof symbol) -> flattened-language-pict-info
(define (flatten-grammar-info info all-nts)
  (let ([union? (extend-language-show-union)])
    (let loop ([info info])
      (cond
        [(vector? info) 
         (let ([orig (loop (vector-ref info 0))]
               [extensions (vector-ref info 1)])
           (if union?
               (map (λ (orig-line)
                      (let* ([nt (car orig-line)]
                             [extension (assoc nt extensions)])
                        (if extension
                            (let ([rhss (cdr extension)])
                              (cons nt
                                    (map (λ (x) 
                                           (if (and (lw? x) (eq? '.... (lw-e x)))
                                               (struct-copy lw
                                                            x
                                                            [e
                                                             (lw->pict all-nts
                                                                       (find-enclosing-loc-wrapper
                                                                        (add-bars (cdr orig-line))))])
                                               x))
                                         (cdr extension))))
                            orig-line)))
                    orig)
               extensions))]
        [else info]))))

(define (make-::=) (basic-text " ::= " (default-style)))
(define (make-bar) 
  (basic-text " | " (default-style))
  #;
  (let ([p (basic-text " | " (default-style))])
    (dc 
     (λ (dc dx dy)
       (cond
         [(is-a? dc post-script-dc%)
          (let ([old-pen (send dc get-pen)])
            (send dc set-pen "black" .6 'solid)
            (send dc draw-line 
                  (+ dx (/ (pict-width p) 2)) dy
                  (+ dx (/ (pict-width p) 2)) (+ dy (pict-height p)))
            (send dc set-pen old-pen))]
         [else
          (send dc draw-text " | " dx dy)]))
     (pict-width p)
     (pict-height p)
     (pict-ascent p)
     (pict-descent p))))

(define (add-bars-and-::= lst)
  (cond
    [(null? lst) null]
    [else
     (cons
      (let ([fst (car lst)])
        (build-lw
         (rc-superimpose (ghost (make-bar)) (make-::=))
         (lw-line fst)
         (lw-line-span fst)
         (lw-column fst)
         0))
      (let loop ([fst (car lst)]
                 [rst (cdr lst)])
        (cond
          [(null? rst) (list fst)]
          [else 
           (let* ([snd (car rst)]
                  [bar 
                   (cond
                     [(= (lw-line snd)
                         (lw-line fst))
                      (let* ([line (lw-line snd)]
                             [line-span (lw-line-span snd)]
                             [column (+ (lw-column fst)
                                        (lw-column-span fst))]
                             [column-span
                              (max (- (lw-column snd)
                                      (+ (lw-column fst)
                                         (lw-column-span fst)))
                                   0)])
                        (build-lw (make-bar) line line-span column column-span))]
                     [else
                      (build-lw
                       (rc-superimpose (make-bar) (ghost (make-::=)))
                       (lw-line snd)
                       (lw-line-span snd)
                       (lw-column snd)
                       0)])])
             (list* fst
                    bar
                    (loop snd (cdr rst))))])))]))

(define (add-bars lst)
  (let loop ([fst (car lst)]
             [rst (cdr lst)])
    (cond
      [(null? rst) (list fst)]
      [else 
       (let* ([snd (car rst)]
              [bar 
               (cond
                 [(= (lw-line snd)
                     (lw-line fst))
                  (let* ([line (lw-line snd)]
                         [line-span (lw-line-span snd)]
                         [column (+ (lw-column fst)
                                    (lw-column-span fst))]
                         [column-span
                          (- (lw-column snd)
                             (+ (lw-column fst)
                                (lw-column-span fst)))])
                    (build-lw (make-bar) line line-span column column-span))]
                 [else
                  (build-lw
                   (make-bar)
                   (lw-line snd)
                   (lw-line-span snd)
                   (lw-column snd)
                   0)])])
         (list* fst
                bar
                (loop snd (cdr rst))))])))


;                                                                                                          
;                                                                                                          
;                                                                                                          
;                            ;              ;;;                                 ;    ;;                    
;                           ;;             ;;;;                                ;;    ;;                    
;  ;;;;;;; ;;;;    ;;;    ;;;;; ;;;;;;;   ;;;;; ;;;; ;;;; ;;;; ;;;    ;;;;;  ;;;;;        ;;;;   ;;;; ;;;  
;  ;;;;;;;;;;;;;  ;;;;;  ;;;;;; ;;;;;;;;  ;;;;  ;;;; ;;;; ;;;;;;;;;  ;;;;;; ;;;;;; ;;;;  ;;;;;;  ;;;;;;;;; 
;  ;;;; ;;; ;;;; ;;;; ;;  ;;;;      ;;;; ;;;;;; ;;;; ;;;; ;;;; ;;;; ;;;;;;;  ;;;;  ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;; ;;; ;;;; ;;;;;;;  ;;;;   ;;;;;;; ;;;;;; ;;;; ;;;; ;;;; ;;;; ;;;;     ;;;;  ;;;; ;;;; ;;; ;;;; ;;;; 
;  ;;;; ;;; ;;;; ;;;;;    ;;;;; ;;  ;;;;  ;;;;  ;;;; ;;;; ;;;; ;;;; ;;;;;;;  ;;;;; ;;;; ;;;;;;;; ;;;; ;;;; 
;  ;;;; ;;; ;;;;  ;;;;;;  ;;;;; ;;;;;;;;  ;;;;  ;;;;;;;;; ;;;; ;;;;  ;;;;;;  ;;;;; ;;;;  ;;;;;;  ;;;; ;;;; 
;  ;;;; ;;; ;;;;   ;;;;    ;;;;  ;; ;;;;  ;;;;   ;;; ;;;; ;;;; ;;;;   ;;;;;   ;;;; ;;;;   ;;;;   ;;;; ;;;; 
;                                                                                                          
;                                                                                                          
;                                                                                                          

(define (make-=) (basic-text " = " (default-style)))

(define-syntax (metafunction->pict stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     #'(metafunctions->pict name)]))

(define-syntax (metafunctions->pict stx)
  (syntax-case stx ()
    [(_ name1 name2 ...)
     (and (identifier? #'name1)
          (andmap identifier? (syntax->list #'(name2 ...))))
     #'(metafunctions->pict/proc (list (metafunction name1) (metafunction name2) ...) 'metafunctions->pict)]))

(define-syntax (render-metafunctions stx)
  (syntax-case stx ()
    [(_ name1 name2 ...)
     (and (identifier? #'name)
          (andmap identifier? (syntax->list #'(name2 ...))))
     #'(render-metafunction/proc (list (metafunction name1) (metafunction name2) ...) #f 'render-metafunctions)]
    [(_ name1 name2 ... #:file filename)
     (and (identifier? #'name1)
          (andmap identifier? (syntax->list #'(name2 ...))))
     #'(render-metafunction/proc (list (metafunction name1) (metafunction name2) ...) filename 'render-metafunctions)]))

(define-syntax (render-metafunction stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     #'(render-metafunction/proc (list (metafunction name)) #f 'render-metafunction)]
    [(_ name file)
     (identifier? #'name)
     #'(render-metafunction/proc (list (metafunction name)) file 'render-metafunction)]))

(define linebreaks (make-parameter #f))

(define metafunction-pict-style (make-parameter 'left-right))
(define metafunction-cases (make-parameter #f))
(define (select-cases eqns)
  (let ([cases (metafunction-cases)])
    (if cases
        (let loop ([eqns eqns]
                   [cases (remove-dups (sort cases <))]
                   [i 0])
          (cond
            [(null? eqns) null]
            [(null? cases) null]
            [else 
             (cond
               [(= i (car cases))
                (cons (car eqns)
                      (loop (cdr eqns) (cdr cases) (+ i 1)))]
               [else
                (loop (cdr eqns) cases (+ i 1))])]))
        eqns)))

;; remove-dups : (listof number)[sorted] -> (listof number)[sorted]
;; removes duplicate numbers from 'l'
(define (remove-dups l)
  (let loop ([l l])
    (cond
      [(null? (cdr l)) l]
      [(= (car l) (cadr l))
       (loop (cdr l))]
      [else
       (cons (car l) (loop (cdr l)))])))

(define (metafunctions->pict/proc mfs name)
  (unless (andmap (λ (mf) (eq? (metafunc-proc-lang (metafunction-proc (car mfs)))
                               (metafunc-proc-lang (metafunction-proc mf))))
                  mfs)
    (error name "expected metafunctions that are all drawn from the same language"))
  (let* ([current-linebreaks (linebreaks)]
         [all-nts (language-nts (metafunc-proc-lang (metafunction-proc (car mfs))))]
         [sep 2]
         [style (metafunction-pict-style)]
         [wrapper->pict (lambda (lw) (lw->pict all-nts lw))]
         [all-eqns (apply append (map (λ (mf) (metafunc-proc-pict-info (metafunction-proc mf))) mfs))]
         [all-lhss 
          (apply append
                 (map (λ (mf)
                        (map (lambda (eqn) 
                               (wrapper->pict
                                (metafunction-call (metafunc-proc-name (metafunction-proc mf))
                                                   (list-ref eqn 0)
                                                   (metafunc-proc-multi-arg? (metafunction-proc mf)))))
                             (metafunc-proc-pict-info (metafunction-proc mf))))
                      mfs))]
         [eqns (select-cases all-eqns)]
         [lhss (select-cases all-lhss)]
         [rhss (map (lambda (eqn) (wrapper->pict (list-ref eqn 2))) eqns)]
         [linebreak-list (or current-linebreaks
                             (map (lambda (x) #f) eqns))]
         [=-pict (make-=)]
         [max-lhs-w (apply max (map pict-width lhss))]
         [max-line-w/pre-sc (apply
                             max
                             (map (lambda (lhs rhs linebreak?)
                                    (max
                                     (if (or linebreak?
                                             (memq style '(up-down
                                                           up-down/vertical-side-conditions
                                                           up-down/compact-side-conditions)))
                                         (max (pict-width lhs)
                                              (+ (pict-width rhs) (pict-width =-pict)))
                                         (+ (pict-width lhs) (pict-width rhs) (pict-width =-pict)
                                            (* 2 sep)))))
                                  lhss rhss linebreak-list))]
         [scs (map (lambda (eqn)
                     (let ([scs (reverse (list-ref eqn 1))])
                     (if (null? scs)
                         #f
                         (let-values ([(fresh where/sc) (partition metafunc-extra-fresh? scs)])
                           (side-condition-pict (foldl (λ (clause picts) 
                                                          (foldr (λ (l ps) (cons (wrapper->pict l) ps))
                                                                 picts (metafunc-extra-fresh-vars clause)))
                                                       '() fresh)
                                                (map (match-lambda
                                                      [(struct metafunc-extra-where (lhs rhs))
                                                       (cons (wrapper->pict lhs) (wrapper->pict rhs))]
                                                      [(struct metafunc-extra-side-cond (expr))
                                                       (wrapper->pict expr)])
                                                     where/sc)
                                                (if (memq style '(up-down/vertical-side-conditions
                                                                  left-right/vertical-side-conditions
                                                                  left-right*/vertical-side-conditions))
                                                    0
                                                    (if (memq style '(up-down/compact-side-conditions
                                                                      left-right/compact-side-conditions
                                                                      left-right*/compact-side-conditions))
                                                        max-line-w/pre-sc
                                                        +inf.0)))))))
                   eqns)])
    (case style
      [(left-right left-right/vertical-side-conditions left-right/compact-side-conditions left-right/beside-side-conditions)
       (table 3
              (apply append
                     (map (lambda (lhs sc rhs linebreak?)
                            (append
                             (if linebreak?
                                 (list lhs (blank) (blank))
                                 (if (and sc (eq? style 'left-right/beside-side-conditions))
                                     (list lhs =-pict (htl-append 10 rhs sc))
                                     (list lhs =-pict rhs)))
                             (if linebreak?
                                 (let ([p rhs])
                                   (list (htl-append sep
                                                     =-pict
                                                     (inset p 0 0 (- 5 (pict-width p)) 0))
                                         (blank)
                                         ;; n case this line sets the max width, add suitable space in the right:
                                         (blank (max 0 (- (pict-width p) max-lhs-w sep))
                                                0)))
                                 null)
                             (if (or (not sc)
                                     (and (not linebreak?)
                                          (eq? style 'left-right/beside-side-conditions)))
                                 null
                                 (list (inset sc 0 0 (- 5 (pict-width sc)) 0)
                                       (blank)
                                       ;; In case sc set the max width...
                                       (blank (max 0 (- (pict-width sc) max-lhs-w (pict-width =-pict) (* 2 sep)))
                                              0)))))
                          lhss
                          scs
                          rhss
                          linebreak-list))
              ltl-superimpose ltl-superimpose
              sep sep)]
      [(up-down up-down/vertical-side-conditions up-down/compact-side-conditions)
       (panorama
        ;; the side-conditions may hang outside the pict, so bring them back w/ panorama
        (apply vl-append
               sep
               (apply append
                      (map (lambda (lhs sc rhs)
                             (cons
                              (vl-append (htl-append lhs =-pict) rhs)
                              (if (not sc)
                                  null
                                  (list (inset sc 0 0 (- 5 (pict-width sc)) 0)))))
                           lhss
                           scs
                           rhss))))])))

(define (metafunction-call name an-lw flattened?)
  (if flattened?
      (struct-copy lw an-lw
                   [e
                    (list*
                     ;; the first loc wrapper is just there to make the
                     ;; shape of this line be one that the apply-rewrites
                     ;; function (in core-layout.ss) recognizes as a metafunction
                     (make-lw "("
                              (lw-line an-lw)
                              0
                              (lw-column an-lw)
                              0 
                              #f
                              #f)
                     (make-lw name
                              (lw-line an-lw)
                              0
                              (lw-column an-lw)
                              0 
                              #f
                              #t)
                     (cdr (lw-e an-lw)))])
      
      (build-lw
       (list
        (build-lw "("
                  (lw-line an-lw)
                  0
                  (lw-column an-lw)
                  0)
        (make-lw name
                 (lw-line an-lw)
                 0
                 (lw-column an-lw)
                 0
                 #f
                 #t)
        an-lw
        (build-lw ")"
                  (+ (lw-line an-lw)
                     (lw-line-span an-lw))
                  0
                  (+ (lw-column an-lw)
                     (lw-column-span an-lw))
                  0))
       (lw-line an-lw)
       (lw-line-span an-lw)
       (lw-column an-lw)
       (lw-column-span an-lw))))  

(define (add-commas-and-rewrite-parens eles)
  (let loop ([eles eles]
             [between-parens? #f]
             [comma-pending #f])
    (cond
      [(null? eles) null]
      [else 
       (let ([an-lw (car eles)])
         (cond
           [(not (lw? an-lw)) 
            (cons an-lw (loop (cdr eles) between-parens? #f))]
           [(equal? "(" (lw-e an-lw))
            (cons (struct-copy lw
                               an-lw
                               [e (open-white-square-bracket)])
                  (loop (cdr eles) #t #f))]
           [(equal? ")" (lw-e an-lw))
            (cons (struct-copy lw
                               an-lw
                               [e (close-white-square-bracket)])
                  (loop (cdr eles) #f #f))]
           [(and between-parens?
                 comma-pending)
            (list* (build-lw (basic-text ", " (default-style))
                             (car comma-pending)
                             0
                             (cdr comma-pending)
                             0)
                   'spring
                   (loop eles #t #f))]
           [else
            (cons an-lw 
                  (loop (cdr eles)
                        between-parens?
                        (if between-parens?
                            (cons (+ (lw-line an-lw) (lw-line-span an-lw))
                                  (+ (lw-column an-lw) (lw-column-span an-lw)))
                            #f)))]))])))

(define (replace-paren x)
  (cond
    [(not (lw? x)) x]
    [(equal? "(" (lw-e x))
     (struct-copy lw
                  x
                  [e (hbl-append -2 
                                 (basic-text "[" (default-style))
                                 (basic-text "[" (default-style)))])]
    [(equal? ")" (lw-e x))
     (struct-copy lw
                  x
                  [e
                   (hbl-append -2 
                               (basic-text "]" (default-style))
                               (basic-text "]" (default-style)))])]
    [else x]))

(define (render-metafunction/proc mfs filename name)
  (cond
    [filename
     (save-as-ps (λ () (metafunctions->pict/proc mfs name))
                 filename)]
    [else
     (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
       (metafunctions->pict/proc mfs name))]))
     

;                              
;                              
;                              
;                              
;    ;                         
;    ;                         
;   ;;;     ;;;   ; ;;   ;;;;; 
;    ;     ;   ;  ;;  ;  ; ; ; 
;    ;     ;;;;;  ;   ;  ; ; ; 
;    ;     ;      ;      ; ; ; 
;    ;     ;   ;  ;      ; ; ; 
;     ;;    ;;;   ;      ; ; ; 
;                              
;                              
;

(define-syntax (render-term stx)
  (syntax-case stx ()
    [(_ lang term)
     #'(render-term/proc lang (to-lw term))]
    [(_ lang term filename)
     #'(render-term/proc lang (to-lw term) filename)]))

(define-syntax (term->pict stx)
  (syntax-case stx ()
    [(_ lang term)
     #'(do-term->pict lang (to-lw term))]))

(define (render-term/proc lang lw [filename #f])
  (if filename
      (save-as-ps (λ () (do-term->pict lang lw)) filename)
      (parameterize ([dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1))])
        (do-term->pict lang lw))))

(define (do-term->pict lang lw) (lw->pict (language-nts lang) lw))
