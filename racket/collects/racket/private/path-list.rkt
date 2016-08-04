(module path-list '#%kernel
  (#%require "qq-and-or.rkt" "define-et-al.rkt")

  (#%provide path-list-string->path-list)

  (define-values (path-list-string->path-list)
    (let ((r (byte-regexp (string->bytes/utf-8
			   (let ((sep (if (eq? (system-type) 'windows)
                                          ";"
                                          ":")))
			     (format "([^~a]*)~a(.*)" sep sep)))))
	  (cons-path (lambda (default s l) 
                       (let ([s (if (eq? (system-type) 'windows)
                                    (regexp-replace* #rx#"\"" s #"")
                                    s)])
                         (if (bytes=? s #"")
                             (append default l)
                             (cons (bytes->path s)
                                   l))))))
      (lambda (s default)
	(unless (or (bytes? s)
		    (string? s))
	  (raise-argument-error 'path-list-string->path-list "(or/c bytes? string?)" s))
	(unless (and (list? default)
		     (andmap path? default))
	  (raise-argument-error 'path-list-string->path-list "(listof path?)" default))
	(let loop ([s (if (string? s)
			  (string->bytes/utf-8 s)
			  s)])
	  (let ([m (regexp-match r s)])
	    (if m
		(cons-path default (cadr m) (loop (caddr m)))
		(cons-path default s null))))))))  
