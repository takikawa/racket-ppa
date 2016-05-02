#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     (only-in racket/list append* remove-duplicates)
                     racket/sequence
                     syntax/parse/pre
                     syntax/parse/experimental/template
                     racket/lazy-require
                     syntax/parse/lib/function-header))

(begin-for-syntax
 (lazy-require [racket/match/patterns (bound-vars pats->bound-vars)]
               [racket/match/gen-match (go go/one)]))

(provide define-forms)

(define-syntax-rule (define-forms parse-id
                      match match* match-lambda match-lambda*
		      match-lambda** match-let match-let*
                      match-let-values match-let*-values
		      match-define match-define-values
                      match-letrec match-letrec-values
		      match/values match/derived match*/derived
                      define/match)
  (...
   (begin
     (provide match match* match-lambda match-lambda* match-lambda**
	      match-let match-let* match-let-values match-let*-values
              match-define match-define-values
              match-letrec match-letrec-values
	      match/values match/derived match*/derived match-define-values
              define/match)
     (define-syntax (match* stx)
       (syntax-parse stx
         [(_ es . clauses)
          (go parse-id stx #'es #'clauses)]))

     (define-syntax (match*/derived stx)
       (syntax-parse stx
         [(_ es orig-stx . clauses)
          (go parse-id #'orig-stx #'es #'clauses)]))

     (define-syntax (match stx)
       (syntax-parse stx
         [(_ arg:expr clauses ...)
          (go/one parse-id stx #'arg #'(clauses ...))]))

     (define-syntax (match/derived stx)
       (syntax-parse stx
         [(_ arg:expr orig-stx clauses ...)
          (go/one parse-id #'orig-stx #'arg #'(clauses ...))]))

     (define-syntax (match/values stx)
       (syntax-parse stx
         [(_ arg:expr (~and cl0 [(pats ...) rhs ...]) clauses ...)
          (with-syntax ([(ids ...) (generate-temporaries #'(pats ...))])
            (quasisyntax/loc stx
              (let-values ([(ids ...) arg])
                (match*/derived (ids ...) #,stx cl0 clauses ...))))]))

     (define-syntax (match-lambda stx)
       (syntax-parse stx
         [(_ . clauses)
          (with-syntax* ([arg (generate-temporary)]
                         [body #`(match/derived arg #,stx . clauses)])
            (syntax/loc stx (lambda (arg) body)))]))

     (define-syntax (match-lambda* stx)
       (syntax-parse stx
         [(_ . clauses)
          (with-syntax* ([arg (generate-temporary)]
                         [body #`(match/derived arg #,stx . clauses)])
            (syntax/loc stx (lambda arg body)))]))

     (define-syntax (match-lambda** stx)
       (syntax-parse stx
         [(_ (~and clauses [(pats ...) . rhs]) ...)
          (with-syntax* ([vars (generate-temporaries (car (syntax-e #'((pats ...) ...))))]
                         [body #`(match*/derived vars #,stx clauses ...)])
            (syntax/loc stx (lambda vars body)))]))


     (define-syntax (match-let-values stx)
       (syntax-parse stx
         [(_ (~and clauses ([(patss ...) rhss:expr] ...)) body1 body ...)
          (define-values (idss let-clauses)
            (for/lists (idss let-clauses)
                ([pats (syntax->list #'((patss ...) ...))]
                 [rhs (syntax->list #'(rhss ...))])
              (define ids (generate-temporaries pats))
              (values ids #`[#,ids #,rhs])))
          (quasisyntax/loc stx
            (let-values #,let-clauses
              (match*/derived #,(append* idss) #,stx
                [(patss ... ...) (let () body1 body ...)])))]))

     (define-syntax (match-let*-values stx)
       (syntax-parse stx
         [(_ () body1 body ...)
          (syntax/loc stx (let () body1 body ...))]
         [(_ ([(pats ...) rhs] rest-pats ...) body1 body ...)
          (with-syntax ([(ids ...) (generate-temporaries #'(pats ...))])
            (quasisyntax/loc stx
              (let-values ([(ids ...) rhs])
                (match*/derived (ids ...) #,stx
                  [(pats ...) #,(syntax/loc stx
                                  (match-let*-values (rest-pats ...)
                                    body1 body ...))]))))]))

     ;; there's lots of duplication here to handle named let
     ;; some factoring out would do a lot of good
     (define-syntax (match-let stx)
       (syntax-parse stx
         [(_ nm:id (~and clauses ([pat init-exp:expr] ...)) body1 body ...)
          (with-syntax*
           ([vars (generate-temporaries #'(pat ...))]
            [loop-body (quasisyntax/loc stx
                         (match*/derived vars #,stx
                                         [(pat ...) (let () body1 body ...)]))])
           (syntax/loc stx
             (letrec ([nm (lambda vars loop-body)])
               (nm init-exp ...))))]
         [(_ ([pat init-exp:expr] ...) body1 body ...)
          (syntax/loc stx (match-let-values ([(pat) init-exp] ...) body1 body ...))]))

     (define-syntax-rule (match-let* ([pat exp] ...) body1 body ...)
       (match-let*-values ([(pat) exp] ...) body1 body ...))

     (define-syntax (match-letrec stx)
       (syntax-parse stx
         [(_ ((~and cl [pat exp]) ...) body1 body ...)
          (quasisyntax/loc stx
			   (let ()
                            #,@(for/list ([c (in-syntax #'(cl ...))]
                                          [p (in-syntax #'(pat ...))]
                                          [e (in-syntax #'(exp ...))])
                                 (quasisyntax/loc c (match-define #,p #,e)))
                            body1 body ...))]))

     (define-syntax (match-letrec-values stx)
       (syntax-parse stx
         [(_ ((~and cl [(pat ...) exp]) ...) body1 body ...)
          (quasisyntax/loc stx
			   (let ()
                            #,@(for/list ([c (in-syntax #'(cl ...))]
                                          [p (in-syntax #'((pat ...) ...))]
                                          [e (in-syntax #'(exp ...))])
                                 (quasisyntax/loc c (match-define-values #,p #,e)))
                            body1 body ...))]))

     (define-syntax (match-define stx)
       (syntax-parse stx
         [(_ pat rhs:expr)
          (let ([p (parse-id #'pat)])
            (with-syntax ([vars (bound-vars p)])
              (quasisyntax/loc stx
                (define-values vars (match*/derived (rhs) #,stx
                                      [(pat) (values . vars)])))))]))

     (define-syntax (match-define-values stx)
       (syntax-parse stx
         [(_ (pats ...) rhs:expr)
          (with-syntax ([(ids ...) (pats->bound-vars parse-id (syntax->list #'(pats ...)))])
            (syntax/loc stx
              (define-values (ids ...)
                (match/values rhs
                  [(pats ...) (values ids ...)]))))]))

     (define-syntax (define/match stx)
       (syntax-parse stx
         [(_ ?header:function-header ?clause ...)
          (template
           (define ?header
             (match* (?? ?header.params)
               ?clause ...)))])))))
