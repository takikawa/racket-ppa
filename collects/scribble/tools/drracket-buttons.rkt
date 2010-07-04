#lang scheme/base

(require scheme/runtime-path
         scheme/gui/base
         scheme/class
         mrlib/bitmap-label
         scheme/system
         setup/xref
         net/sendurl)

(provide drracket-buttons)

(define-runtime-path pdf-png-path "pdf.png")
(define-runtime-path html-png-path "html.png")
(define pdf.png (make-object bitmap% pdf-png-path 'png/mask))
(define html.png (make-object bitmap% html-png-path 'png/mask))

(define-namespace-anchor anchor)

(define (make-render-button label bmp mode suffix extra-cmdline)
  (list 
   label
   bmp
   (λ (drs-frame)
     (let* ([t (send drs-frame get-definitions-text)]
            [fn (send t get-filename)])
       (if (and fn (not (send t is-modified?)))
           (let-values ([(p) (open-output-string)]
                        [(base name dir?) (split-path fn)])
             (parameterize ([current-namespace (make-base-namespace)]
                            [current-output-port p]
                            [current-error-port p]
                            [current-directory base]
                            [current-command-line-arguments
                             (list->vector 
                              (append
                               extra-cmdline
                               (list "--quiet")
                               (list mode (if (path? fn) (path->string fn) fn))))])
               (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'setup/xref)
               (dynamic-require 'scribble/run #f)
               (cond
                 [(equal? suffix #".html")
                  (send-url/file (path-replace-suffix fn suffix))]
                 [else
                  (system (format "open ~s" (path->string (path-replace-suffix fn suffix))))]))
             (let ([s (get-output-string p)])
               (unless (equal? s "")
                 (message-box "Scribble" s drs-frame))))
           (message-box "Not Named" "Cannot render unsaved file"))))))

(define drracket-buttons
  (let ([html-button
         (make-render-button "Scribble HTML" html.png "--html" #".html" 
                             '("++xref-in" "setup/xref" "load-collections-xref"))]
        [pdf-button
         ;; only available on OSX currently
         ;; when we have a general way of opening pdfs, can use that
         (make-render-button "Scribble PDF" pdf.png "--pdf" #".pdf" null)])
    (case (system-type)
      [(macosx) (list html-button pdf-button)]
      [else (list html-button)])))
