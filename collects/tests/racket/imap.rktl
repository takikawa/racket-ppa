
(define imap-server "imap.cs.utah.edu")
(define imap-port-no 993)
(define username "mflatt")
(define pw (with-input-from-file (build-path (current-load-relative-directory) "pw")
	     read))
(define mailbox-name "INBOX.tmp") ;; !!! ALL CONTENT WILL BE DELETED !!!

(load-relative "loadtest.rktl")

(Section 'imap)

(require openssl/mzssl
	 net/imap
	 mzlib/etc)

(define (test-connect)
  (if (zero? (random 2))
      (parameterize ([imap-port-number 993])
        (imap-connect #:tls? #t imap-server username pw mailbox-name))
      (let ([c (ssl-make-client-context)])
        (let-values ([(in out) (ssl-connect imap-server imap-port-no c)])
          (imap-connect* in out username pw mailbox-name)))))

(define-values (imap cnt recent) (test-connect))

(printf "Msgs: ~a; Validity: ~a\n" cnt (imap-uidvalidity imap))

(test cnt imap-messages imap)
(test recent imap-recent imap)
(test #t number? (imap-uidvalidity imap))
(test #f imap-pending-expunges? imap)
(test #f imap-pending-updates? imap)
(test #f imap-new? imap)

(imap-disconnect imap)
(error "comment out these two lines to finish...")

(define (delete-all)
  (let ([cnt (imap-messages imap)])
    (unless (zero? cnt)
      (let ([all (build-list cnt add1)])
	(test (void) imap-store imap '+ all '|\Deleted|)
	(test (void) imap-expunge imap)
	(test #t imap-pending-expunges? imap)
	(test all imap-get-expunges imap)
	(test null imap-get-expunges imap)
	(test #f imap-pending-expunges? imap)))))

(delete-all)
(test #f imap-new? imap)
(test 0 imap-messages imap)
(test '(0 0) 'noop (let-values ([(a b) (imap-noop imap)])
		     (list a b)))
(test (void) imap-poll imap)
(test 0 imap-messages imap)
(test 0 imap-recent imap)

(test #f imap-new? imap)

(define (add-one content total)
  (test (void) imap-append imap mailbox-name content)
  (imap-noop imap)
  (test total imap-messages imap)
  (test #t imap-new? imap)
  (let ([uids (imap-get-messages imap (build-list total add1) '(uid))])
    (test #t list? uids)
    (test total length uids)
    (let ([l (list-ref uids (sub1 total))])
      (test #t list? l)
      (test 1 length l)
      (test #t number? (car l))
      (car l))))
  

(define sample-head #"Subject: Hello\r\n\r\n")
(define sample-body #"Hi there.\r\n")

(let ([uid (add-one (bytes-append sample-head sample-body) 1)])
  (test (list (list uid 
		    sample-head
		    sample-body
		    '(|\Seen|)))
	imap-get-messages imap '(1) '(uid header body flags)))
(test (void) imap-store imap '+ '(1) (list (symbol->imap-flag 'answered)))
(test (list '((|\Answered| |\Seen|))) imap-get-messages imap '(1) '(flags))
(test (void) imap-store imap '- '(1) (list (symbol->imap-flag 'answered)))
(test (list '((|\Seen|))) imap-get-messages imap '(1) '(flags))
(test (void) imap-store imap '+ '(1) (list (symbol->imap-flag 'deleted)))
(test (list '((|\Seen| |\Deleted|))) imap-get-messages imap '(1) '(flags))
(test (void) imap-store imap '! '(1) (list (symbol->imap-flag 'answered)))
(test (list '((|\Answered|))) imap-get-messages imap '(1) '(flags))

(test #f imap-pending-updates? imap)
(test null imap-get-updates imap)

;; Test multiple-client access:
(let ()
  (define-values (imap2 cnt2 recent2) (test-connect))
  (test '(1 0) list cnt2 recent2)
  
  (let ([uid (add-one (bytes-append sample-head sample-body) 2)])
    (let loop ([n 5])
      (when (zero? n)
	(imap-noop imap2))
      (imap-poll imap2)
      (unless (imap-new? imap2)
	(sleep 0.2)
	(loop (sub1 n))))
    (test #t imap-new? imap2)
    (test 2 imap-messages imap2)
    (let ([uids (imap-get-messages imap2 '(2) '(uid))])
      (test uid caar uids)))

  ;; Delete message on imap2, check notifcation to imap
  (test (void) imap-store imap2 '+ '(2) (list (symbol->imap-flag 'deleted)))
  (test (void) imap-expunge imap2)
  (test '(2) imap-get-expunges imap2)
  (imap-noop imap)
  (err/rt-test (imap-store imap '+ '(2) (list (symbol->imap-flag 'answered))))
  (test #t imap-pending-expunges? imap)
  (test '(2) imap-get-expunges imap)

  ;; Adjust flags on imap2, check notifcation to imap
  (test #f imap-pending-updates? imap)
  (test (void) imap-store imap2 '+ '(1) (list (symbol->imap-flag 'deleted)))
  (imap-noop imap)
  (test #t imap-pending-updates? imap)
  (test #t list? (imap-get-updates imap))
  (test #f imap-pending-updates? imap)

  ;; Check that multiple updates are collapsed:
  (test (void) imap-store imap2 '- '(1) (list (symbol->imap-flag 'deleted)))  
  (imap-noop imap)
  (test #t imap-pending-updates? imap)
  (test (void) imap-store imap2 '+ '(1) (list (symbol->imap-flag 'deleted)))
  (test (void) imap-store imap2 '- '(1) (list (symbol->imap-flag 'deleted)))
  (imap-noop imap)
  (test #t imap-pending-updates? imap)
  (test 1 length (imap-get-updates imap))
  
  (test (void) imap-reset-new! imap2)
  (add-one (bytes-append sample-head sample-body) 2)
  (add-one (bytes-append sample-head sample-body) 3)
  (add-one (bytes-append sample-head sample-body) 4)
  (add-one (bytes-append sample-head sample-body) 5)
  (imap-noop imap2)
  (test #t imap-new? imap2)
  (test 5 imap-messages imap)
  (test 5 imap-messages imap2)

  (test #t list? (imap-get-messages imap '(1 2 3 4 5) '(uid)))
  (test #t list? (imap-get-messages imap2 '(1 2 3 4 5) '(uid)))
  
  ;; Test deleteing multiple messages, and shifts in flag updates
  (test (void) imap-store imap2 '+ '(2 4) (list (symbol->imap-flag 'deleted)))
  (test (void) imap-store imap2 '+ '(3 5) (list (symbol->imap-flag 'answered)))
  (test (void) imap-expunge imap2)
  (imap-noop imap)
  (imap-noop imap2)
  (test #t imap-pending-expunges? imap)
  (test #t imap-pending-expunges? imap2)
  (test '(2 4) imap-get-expunges imap)
  (test '(2 4) imap-get-expunges imap2)
  (test #t imap-pending-updates? imap)
  (test '(2 3) map car (imap-get-updates imap))

  (imap-disconnect imap2))

(imap-disconnect imap)

(report-errs)
