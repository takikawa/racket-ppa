
(define (read-linklet-bundle-hash in)
  (performance-region
   'read-linklet
   (let* ([len (integer-bytes->integer (read-bytes 4 in) #f #f)]
          [bstr (read-bytes len in)])
     (adjust-linklet-bundle-laziness-and-paths
      (fasl-read (open-bytevector-input-port bstr))))))

(define read-on-demand-source
  (make-parameter #f
                  (lambda (v)
                    (unless (or (eq? v #t) (eq? v #f) (and (path? v)
                                                           (complete-path? v)))
                      (raise-argument-error 'read-on-demand-source
                                            "(or/c #f #t (and/c path? complete-path?))"
                                            v))
                    v)))

(define (adjust-linklet-bundle-laziness-and-paths ht)
  (let loop ([i (hash-iterate-first ht)])
    (cond
     [(not i) (hasheq)]
     [else
      (let-values ([(key val) (hash-iterate-key+value ht i)])
        (hash-set (loop (hash-iterate-next ht i))
                  key
                  (if (linklet? val)
                      (adjust-linklet-laziness
                       (decode-linklet-paths val))
                      val)))])))

(define (adjust-linklet-laziness linklet)
  (set-linklet-code linklet
                    (linklet-code linklet)
                    (cond
                     [(not (eq? root-inspector (|#%app| current-code-inspector)))
                      ;; Originally, the idea was that bytecode can be loaded in
                      ;; a non-original code inspector as long as it doesn't refer
                      ;; to unsafe operation. But increasing use of compilation to
                      ;; unsafe operations, not to mention compilation to machine
                      ;; code, means that all "bytecode" is unsafe:
                      'faslable-unsafe]
                     [(|#%app| read-on-demand-source)
                      ;; Remember that the linklet can be lazier:
                      'faslable]
                     [else
                      'faslable-strict])))

(define (decode-linklet-paths linklet)
  (let ([paths (linklet-paths linklet)])
    (cond
     [(null? paths)
      linklet]
     [else
      (set-linklet-paths linklet (map compiled-path->path paths))])))
