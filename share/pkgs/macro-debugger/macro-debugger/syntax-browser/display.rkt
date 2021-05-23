#lang racket/base
(require racket/class
         racket/gui/base
         racket/promise
         racket/match
         data/interval-map
         framework
         racket/class/iop
         "pretty-printer.rkt"
         macro-debugger/syntax-browser/interfaces
         "prefs.rkt"
         "util.rkt"
         "../util/logger.rkt"
         (only-in "icons/lock.rkt" lock-icon-snip%)
         (only-in "icons/tainted.rkt" tainted-icon-snip%))
(provide print-syntax-to-editor
         code-style)

(define-syntax-rule (uninterruptible e ...)
  ;; (coarsely) prevent breaks within editor operations
  (parameterize-break #f (begin e ...)))

(define (now) (current-inexact-milliseconds))

;; FIXME: assumes text never moves

;; print-syntax-to-editor : syntax text controller<%> config number number
;;                       -> display<%>
;; Note: must call display<%>::refresh to finish styling.
(define (print-syntax-to-editor stx text controller config columns
                                [insertion-point (send text last-position)])
  (define output-port (open-output-string/count-lines))
  (define taint-icon-locs (and (send/i config config<%> get-taint-icons)
                               (if (replace-taint-icons? config text) (box null) #t)))
  (define range
    (with-log-time "** pretty-print-syntax"
      (pretty-print-syntax stx output-port 
                           (send/i controller controller<%> get-primary-partition)
                           (length (send/i config config<%> get-colors))
                           (send/i config config<%> get-suffix-option)
                           (send config get-pretty-styles)
                           columns
                           (send config get-pretty-abbrev?)
                           #:taint-icons taint-icon-locs)))
  (define output-string (get-output-string output-port))
  (define output-length (sub1 (string-length output-string))) ;; skip final newline
  (log-macro-stepper-debug "size of pretty-printed text: ~s" output-length)
  (with-log-time "fixup-parentheses"
    (fixup-parentheses output-string range))
  (with-unlock text
    (with-log-time "inserting pretty-printed text"
      (uninterruptible
       (if (box? taint-icon-locs)
           (insert-string/replace-icons text output-string output-length insertion-point
                                        (reverse (unbox taint-icon-locs)))
           (send text insert output-length output-string insertion-point))))
    (new display%
         (text text)
         (controller controller)
         (config config)
         (range range)
         (start-position insertion-point)
         (end-position (+ insertion-point output-length)))))

(define (replace-taint-icons? config text)
  (let loop ([mode (send config get-taint-icons)])
    (case mode
      [(snip) #t]
      [(char)
       (define font (send (code-style text #f) get-font))
       (not (for/and ([c (in-list '(#\🔒 #\🔓 #\💥))])
              (send font screen-glyph-exists? c)))]
      [else #f])))

;; display%
;; Note: must call refresh method to finish styling.
(define display%
  (class* object% (display<%>)
    (init-field/i [controller controller<%>]
                  [config config<%>]
                  [range range<%>])
    (init-field text
                start-position
                end-position)

    (define base-style
      (code-style text (send/i config config<%> get-syntax-font-size)))

    ;; on-next-refresh : (listof (list stx style-delta boolean)
    ;; Styles to be applied on next refresh only. (eg, underline)
    (define on-next-refresh null)

    ;; extra-styles : hash[stx => (listof style-delta)]
    ;; Styles to be re-applied on every refresh.
    (define extra-styles (make-hasheq))

    ;; to-undo-styles : (listof Range)
    ;; Ranges to unbold or unhighlight when selection changes.
    ;; FIXME: ought to be managed by text:region-data (to auto-update ranges)
    ;;   until then, positions are relative
    (define to-undo-styles null)

    ;; initialize : -> void
    (define/private (initialize)
      (with-log-time "changing base style"
        (uninterruptible
         (send text change-style base-style start-position end-position #f)))
      (with-log-time "applying primary styles"
        (uninterruptible (apply-primary-partition-styles)))
      (when #f
        (with-log-time "autocorrect icon sizes"
          (autocorrect-icon-sizes)))
      (with-log-time "adding clickbacks"
        (uninterruptible (add-clickbacks))))

    (define/private (autocorrect-icon-sizes)
      (define ratio
        (let ([dc (send text get-dc)])
          (define-values (xw _xh _xd _xa) (send dc get-text-extent "x"))
          (define-values (aw _ah _ad _aa) (send dc get-text-extent "🔒"))
          #;(define-values (cw _ch _cd _ca) (send dc get-text-extent "💥"))
          (/ xw (max aw #;cw xw))))
      (when (< ratio 0.90) ;; tolerance
        (with-unlock text
          (define sd (make-object style-delta%))
          (send sd set-size-mult (max ratio 2/3))
          (for ([r (in-list (send/i range range<%> all-ranges))]
                #:when (not (= (range-start r) (range-pstart r))))
            (send text change-style sd
                  (relative->text-position (range-start r))
                  (relative->text-position (range-pstart r)))))))

    ;; add-clickbacks : -> void
    (define/private (add-clickbacks)
      (define mapping (send text get-region-mapping 'syntax))
      (define lazy-interval-map-init
        (delay
          (with-log-time "forcing clickback mapping"
           (uninterruptible
            (for ([range (send/i range range<%> all-ranges)])
              (let ([stx (range-stx range)]
                    [start (range-start range)]
                    [end (range-end range)])
                (interval-map-set! mapping (+ start-position start) (+ start-position end) stx)))))))
      (define (the-callback position)
        (force lazy-interval-map-init)
        (send/i controller selection-manager<%> set-selected-syntax
                (interval-map-ref mapping position #f)))
      (send text set-clickregion start-position end-position the-callback)
      (send text set-clickregion start-position end-position the-callback 'right-down))

    ;; refresh : -> void
    ;; Clears all highlighting and reapplies all non-foreground styles.
    (define/public (refresh)
      (with-log-time "refresh"
       (with-unlock text
        (uninterruptible
         (let ([undo-select/highlight-d (get-undo-select/highlight-d)])
           (for ([r (in-list to-undo-styles)])
             (send text change-style undo-select/highlight-d
                   (relative->text-position (range-start r))
                   (relative->text-position (range-end r)))))
         (set! to-undo-styles null))
        (uninterruptible
         (for ([todo (in-list on-next-refresh)])
           (match-define (list stx delta pstart?) todo)
           (for ([r (in-list (send/i range range<%> get-ranges stx))])
             (restyle-range r delta #f pstart?)))
         (set! on-next-refresh null))
        (uninterruptible
         (apply-extra-styles))
        (let ([selected-syntax
               (send/i controller selection-manager<%>
                       get-selected-syntax)])
          (uninterruptible
           (apply-secondary-relation-styles selected-syntax))
          (uninterruptible
           (apply-selection-styles selected-syntax))))))

    ;; get-range : -> range<%>
    (define/public (get-range) range)

    ;; get-start-position : -> number
    (define/public (get-start-position) start-position)

    ;; get-end-position : -> number
    (define/public (get-end-position) end-position)

    ;; highlight-syntaxes : (list-of syntax) string -> void
    (define/public (highlight-syntaxes stxs hi-color)
      (let ([delta (highlight-style-delta hi-color)])
        (for ([stx (in-list stxs)])
          (hash-set! extra-styles stx
                     (cons delta (hash-ref extra-styles stx null))))))

    ;; underline-syntaxes : (listof syntax) -> void
    (define/public (underline-syntaxes stxs)
      (for ([stx (in-list stxs)])
        (set! on-next-refresh
              (cons (list stx underline-d #t) on-next-refresh))))

    ;; Primary styles
    ;; (Done once on initialization, never repeated)

    ;; apply-primary-partition-styles : -> void
    ;; Changes the foreground color according to the primary partition.
    ;; Only called once, when the syntax is first drawn.
    (define/private (apply-primary-partition-styles)
      (define style-list (send text get-style-list))
      (define (color-style color)
        (let ([delta (new style-delta%)])
          (send delta set-delta-foreground color)
          (send style-list find-or-create-style base-style delta)))
      (define color-styles
        (list->vector
         (map color-style
              (map translate-color
                   (send/i config config<%> get-colors)))))
      (define overflow-style (color-style (translate-color "darkgray")))
      (define color-partition
        (send/i controller controller<%> get-primary-partition))
      (define offset start-position)
      ;; Optimization: don't call change-style when new style = old style
      (let tr*loop ([trs (send/i range range<%> get-treeranges)] [old-style #f])
        (for ([tr trs])
          (define stx (treerange-stx tr))
          (define start (treerange-start tr))
          (define end (treerange-end tr))
          (define subs (treerange-subs tr))
          (define new-style
            (primary-style stx color-partition color-styles overflow-style))
          (unless (eq? old-style new-style)
            (send text change-style new-style (+ offset start) (+ offset end) #f))
          (tr*loop subs new-style)))
      (void))

    ;; primary-style : syntax partition (vector-of style-delta%) style-delta%
    ;;               -> style-delta%
    (define/private (primary-style stx partition color-vector overflow)
      (let ([n (send/i partition partition<%> get-partition stx)])
        (cond [(< n (vector-length color-vector))
               (vector-ref color-vector n)]
              [else
               overflow])))

    ;; Secondary Styling
    ;; May change in response to user actions

    ;; apply-extra-styles : -> void
    ;; Applies externally-added styles (such as highlighting)
    (define/private (apply-extra-styles)
      (for ([(stx deltas) (in-hash extra-styles)])
        (for ([r (in-list (send/i range range<%> get-ranges stx))])
          (for ([delta (in-list deltas)])
            (restyle-range r delta #t)))))

    ;; apply-selection-styles : syntax -> void
    ;; Styles subterms eq to the selected syntax
    (define/private (apply-selection-styles selected-syntax)
      (for ([r (in-list (send/i range range<%> get-ranges selected-syntax))])
        (restyle-range r select-d #t)))

    ;; apply-secondary-relation-styles : selected-syntax -> void
    ;; If the selected syntax is an identifier, then styles all identifiers
    ;; in the relation with it.
    (define/private (apply-secondary-relation-styles selected-syntax)
      (when (identifier? selected-syntax)
        (let* ([relation (send/i controller controller<%> get-identifier=?)]
               [secondary-highlight-d (get-secondary-highlight-d)])
          (when relation
            (for ([id (in-list (send/i range range<%> get-identifier-list))])
              (when (relation selected-syntax id)
                (for ([r (in-list (send/i range range<%> get-ranges id))])
                  (restyle-range r secondary-highlight-d #t))))))))

    ;; restyle-range : Range style-delta% boolean -> void
    (define/private (restyle-range r style need-undo? [pstart? #f])
      (when need-undo? (set! to-undo-styles (cons r to-undo-styles)))
      (send text change-style style
            (relative->text-position (if pstart? (range-pstart r) (range-start r)))
            (relative->text-position (range-end r))))

    ;; relative->text-position : number -> number
    (define/private (relative->text-position pos)
      (+ pos start-position))

    ;; Initialize
    (super-new)
    (send/i controller controller<%> add-syntax-display this)
    (initialize)))

;; fixup-parentheses : string range -> void
(define (fixup-parentheses string range)
  (for ([r (send/i range range<%> all-ranges)])
    (let ([stx (range-stx r)]
          [start (range-start r)]
          [end (range-end r)])
      (when (and (syntax? stx) (pair? (syntax-e stx)))
        (case (syntax-property stx 'paren-shape)
          ((#\[)
           (string-set! string start #\[)
           (string-set! string (sub1 end) #\]))
          ((#\{) 
           (string-set! string start #\{)
           (string-set! string (sub1 end) #\})))))))

(define (insert-string/replace-icons text s len insertion-point locs)
  (define (loop start locs) ;; already processed s up to start
    (cond [(null? locs)
           (flush start len)]
          [else
           (flush start (car locs))
           (case (string-ref s (car locs))
             [(#\🔒 #\🔓) ;; LOCK, OPEN LOCK
              (send text insert (new lock-icon-snip%) (+ insertion-point (car locs)))]
             [(#\💥) ;; COLLISION SYMBOL
              (send text insert (new tainted-icon-snip%) (+ insertion-point (car locs)))]
             [else
              (send text insert (string-ref s (car locs)) (+ insertion-point (car locs)))])
           (loop (add1 (car locs)) (cdr locs))]))
  (define (flush start end)
    (when (< start end)
      (send text insert (substring s start end) (+ insertion-point start))))
  (loop 0 locs))

(define (open-output-string/count-lines)
  (let ([os (open-output-string)])
    (port-count-lines! os)
    os))

;; code-style : text<%> number/#f -> style<%>
(define (code-style text font-size)
  (let* ([style-list (send text get-style-list)]
         [style (send style-list find-named-style (editor:get-default-color-style-name))])
    (if font-size
        (send style-list find-or-create-style
              style
              (make-object style-delta% 'change-size font-size))
        style)))

;; Color translation

;; translate-color : color-string -> color%
(define (translate-color color-string)
  (let ([c (make-object color% color-string)])
    (if (pref:invert-colors?)
        (let-values ([(r* g* b*)
                      (lightness-invert (send c red) (send c green) (send c blue))])
          #|
          (printf "translate: ~s -> ~s\n"
                  (list (send c red) (send c green) (send c blue))
                  (list r* g* b*))
          |#
          (make-object color% r* g* b*))
        c)))

;; lightness-invert : uint8 uint8 uint8 -> (values uint8 uint8 uint8)
(define (lightness-invert r g b)
  (define (c x)
    (/ (exact->inexact x) 255.0))
  (define (d x)
    (inexact->exact (round (* x 255))))
  (let-values ([(r g b) (lightness-invert* (c r) (c g) (c b))])
    (values (d r) (d g) (d b))))

(define (lightness-invert* R G B)
  (let-values ([(Hp Sl L) (rgb->hsl* R G B)])
    (hsl*->rgb Hp Sl (- 1.0 L))))

(define (rgb->hsl* R G B)
  (define M (max R G B))
  (define m (min R G B))
  (define C (- M m))
  (define Hp
    (cond [(zero? C)
           ;; Undefined, but use 0
           0.0]
          [(= M R)
           (realmod* (/ (- G B) C) 6)]
          [(= M G)
           (+ (/ (- B R) C) 2)]
          [(= M B)
           (+ (/ (- R G) C) 4)]))
  (define L (* 0.5 (+ M m)))
  (define Sl
    (cond [(zero? C) 0.0]
          [(>= L 0.5) (/ C (* 2 L))]
          [else (/ C (- 2 (* 2 L)))]))
  
  (values Hp Sl L))

(define (hsl*->rgb Hp Sl L)
  (define C
    (cond [(>= L 0.5) (* 2 L Sl)]
          [else (* (- 2 (* 2 L)) Sl)]))
  (define X (* C (- 1 (abs (- (realmod Hp 2) 1)))))
  (define-values (R1 G1 B1)
    (cond [(< Hp 1) (values C X 0)]
          [(< Hp 2) (values X C 0)]
          [(< Hp 3) (values 0 C X)]
          [(< Hp 4) (values 0 X C)]
          [(< Hp 5) (values X 0 C)]
          [(< Hp 6) (values C 0 X)]))
  (define m (- L (* 0.5 C)))
  (values (+ R1 m) (+ G1 m) (+ B1 m)))

;; realmod : real integer -> real
;; Adjusts a real number to [0, base]
(define (realmod x base)
  (define xint (ceiling x))
  (define m (modulo xint base))
  (realmod* (- m (- xint x)) base))

;; realmod* : real real -> real
;; Adjusts a number in [-base, base] to [0,base]
;; Not a real mod, but faintly reminiscent.
(define (realmod* x base)
  (if (negative? x)
      (+ x base)
      x))

;; Styles

(define select-d
  (make-object style-delta% 'change-weight 'bold))

(define underline-d
  (make-object style-delta% 'change-underline #t))

(define (highlight-style-delta raw-color #:translate-color? [translate-color? #t])
  (let ([sd (new style-delta%)]
        [color (if translate-color? (translate-color raw-color) raw-color)])
    (send sd set-delta-background color)
    sd))

(define (mk-2-constant-style bow-color [wob-color (translate-color bow-color)])
  (let ([wob-version (highlight-style-delta wob-color #:translate-color? #f)]
        [bow-version (highlight-style-delta bow-color #:translate-color? #f)])
    (λ ()
      (if (pref:invert-colors?)
          wob-version
          bow-version))))

(define get-secondary-highlight-d
  (mk-2-constant-style "yellow" "darkgoldenrod"))

(define (get-undo-select/highlight-d)
  (let ([sd (make-object style-delta% 'change-weight 'normal)]
        [bg (if (pref:invert-colors?) "black" "white")])
    (send sd set-delta-background bg)
    sd))

(define (light-color? c)
  (define-values (R G B) (values (send c red) (send c green) (send c blue)))
  (define LL (+ (max R G B) (min R G B)))
  (> LL 256))

(define invert-colors?
  (light-color?
   (color-prefs:lookup-in-color-scheme 'framework:default-text-color)))

(color-prefs:register-color-scheme-entry-change-callback
 'framework:default-text-color
 (lambda (c) (set! invert-colors? (light-color? c))))

(define (pref:invert-colors?) invert-colors?)
