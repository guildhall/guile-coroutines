(define-module (stis coroutine)
  #:use-module (ice-9 match)
  #:export (with-coroutines-raw 
	    with-coroutines
	    goto
	    gosub

	    return
	    return-from

	    resume
	    resume-from

	    yield
	    yield-from

	    continue

	    consume 	   
	    consume-from

	    transit
	    transit-from

	    feed
	    with-feed

	    define-coroutine
	    define-coroutine-inited
	    tagbody))

(define return-stack (make-fluid '()))

(define-syntax-parameter TAG
  (lambda (x) 
    (error 
     "TAG should be used in the transition part of with-coroutines")))

(define-syntax-parameter TAG
  (lambda (x) 
    (error 
     "TAG should be used in the transition part of with-coroutines")))


(define-syntax-parameter K
  (lambda (x) 
    (error 
     "K should be used in the transition part of with-coroutines")))

(define-syntax-parameter THUNK
  (lambda (x) 
    #'errror-THUNK
    #;(error 
     "THUNK should be used in the transition part of with-coroutines")))

(define-syntax-parameter TH-INIT
  (lambda (x) 
    #'error-TH-INIT
    #;
    (error 
     "TH-INIT should be used in the transition part of with-coroutines")))

(define-syntax-rule (with (init th k) code ...)
  (syntax-parameterize ((K       (lambda x #'k))
			(THUNK   (make-variable-transformer
				  (lambda (x)
					  (syntax-case x (set!)
					    ((set! _ v) #'(set! th v))
					    ((_ . _) 
					     (error "THUNK is not a funcition"))
					    (_ #'th)))))
			(TH-INIT (lambda x #'init)))
     code ...))

(define-syntax-rule (with-tag tag code ...)
  (syntax-parameterize ((TAG (lambda x #'tag)))
    code ...))

(define-syntax-rule (coroutine-reset) (set! THUNK TH-INIT))
(define-syntax-rule (coroutine-cont)  (set! THUNK K))
(define-syntax-rule (coroutine-keep)  (if #f #f))

(define ref-g #f)
(define-syntax-rule (with-g ((g tag) ...) code ...)
  (let-syntax ((g (lambda (x)
		    (syntax-case x (ref-g)
		      ((_ ref-g abort kind . l)
		       #'(abort tag kind g . l))
		      (x (identifier? #'x) 
			 #'g)))) ...)
    code ...))
 
(define-syntax with-coroutines-raw
  (lambda (x)
    (syntax-case x (/.)
      ((_ ((nm thunk 
	       ((exit (pat transition-code ...) ...) ...))
	   ...)
	  code ...)
       
       #`(letrec ((nm  (let* ((tag (make-prompt-tag))
			      (th    
			       (with-tag tag 
				 (with-g #,(map (lambda (nm) #`(#,nm tag))
						#'(nm ...))
				    thunk)))
			      (init th))
			 (lambda a
			   (call-with-prompt tag
			       (lambda () (apply th a))
			       (lambda (k q . args)
				 (cond 
				  ((eq? q exit) 
				   (with (init th k)
					 (match args 
						(pat transition-code ...) ...)))
				  ...))))))
		  ...)
	   code ...)))))

(define-syntax mk-exit
  (lambda (x)
    (syntax-case x ()
      ((_ nm)
       (with-syntax ((s (datum->syntax #'1 (gensym "exit"))))
	  #'(define-syntax nm
	      (lambda (x) #''s)))))))




(mk-exit goto-exit)
(mk-exit gosub-exit)
(mk-exit return-exit)
(mk-exit resume-exit)
(mk-exit yield-exit)
(mk-exit continue-exit)
(mk-exit consume-exit)
(mk-exit transit-exit)

(define-syntax-rule (goto g)      (g ref-g abort-to-prompt goto-exit))
(define-syntax-rule (gosub g)     (g ref-g abort-to-prompt gosub-exit))

(define-syntax-rule (return . l)  (abort-to-prompt TAG return-exit . l))
(define-syntax-rule (return-from g . l)  
  (g ref-g abort-to-prompt TAG return-from-exit . l))

(define-syntax-rule (resume      . l)  (abort-to-prompt TAG resume-exit . l))
(define-syntax-rule (resume-from g . l)  
  (g ref-g abort-to-prompt resume-exit . l))

(define-syntax-rule (yield x ...) (abort-to-prompt TAG yield-exit x ...))
(define-syntax-rule (yield-from g x ...) 
  (g ref-g abort-to-prompt yield-exit x ...))

(define-syntax-rule (continue g)  (g ref-g abort-to-prompt continue-exit))

(define-syntax consume 
  (syntax-rules ()
    ((_ v)   (begin
	       (when (not (pair? v)) 
		     (set! v (abort-to-prompt TAG consume-exit)))
	       (let ((r (car v)))
		 (set! v (cdr v))
		 r)))

    ((_) (abort-to-prompt TAG consume-exit))))

(define-syntax consume-from
  (syntax-rules ()
    ((_ g v)   (begin
		 (when (not (pair? v)) 
		       (set! v (g ref-g abort-to-prompt consume-exit)))
		 (let ((r (car v)))
		   (set! v (cdr v))
		   r)))

    ((_) (g ref-g abort-to-prompt consume-exit))))

(define-syntax transit-from 
  (syntax-rules ()
    ((g (v) data ...)
     (begin
       (set! v (append v (g ref-g abort-to-prompt transit-exit data ...)))
       (let ((r (car v)))
	 (set! v (cdr v))
	 r)))
    ((g () data ...)
     (g ref-g abort-to-prompt transit-exit data ...))))

    

(define-syntax transit 
  (syntax-rules ()
    (((v) data ...)
     (begin
       (set! v (append v (abort-to-prompt TAG transit-exit data ...)))
       (let ((r (car v)))
	 (set! v (cdr v))
	 r)))
    ((() data ...)
     (abort-to-prompt TAG transit-exit data ...))))

(define-syntax-rule (with-coroutines ((nm thunk) ...) code ...)
  (with-coroutines-raw ((nm thunk
			    ((goto-exit  
			      ((g) (coroutine-reset) (g)))
			     
			     (gosub-exit
			      ((g)
			       (coroutine-cont)
			       (fluid-set! return-stack
					   (cons nm (fluid-ref return-stack)))
			       (g)))
			     
			     (return-exit
			      (x 
			       (coroutine-reset)
			       (let ((gs (fluid-ref return-stack)))
				 (if (pair? gs)
				     (let ((g (car gs)))
				       (fluid-set! return-stack (cdr gs))
				       (apply g x))
				     (error "return-stack is empty")))))

			     (return-exit
			      (x 
			       (coroutine-cont)
			       (let ((gs (fluid-ref return-stack)))
				 (if (pair? gs)
				     (let ((g (car gs)))
				       (fluid-set! return-stack (cdr gs))
				       (apply g x))
				     (error "return-stack is empty")))))

			     (yield-exit 
			      (x   (coroutine-cont) (apply values x)))
			     
			     (continue-exit
			      ((g) (coroutine-cont) (g)))

			     (consume-exit
			      (()  (coroutine-cont) (if #f #f)))
			     
			     (transit-exit
			      (x   (coroutine-cont) (apply values x)))))
			...)
       code ...))

(define-syntax-rule (feed g x ...) (g (list x ...)))
(define-syntax-rule (with-feed (q) code ...) (let ((q '())) code ...))

(define-syntax-rule (define-coroutine (name . args) code ...)
  (define name
    (lambda args
      (with-coroutines ((it (lambda () code ...)))
	 it))))

(define-syntax-rule (define-coroutine-inited (name . args) code ...)
  (define name
    (lambda args
      (with-coroutines ((it (lambda () code ...)))
	(it) it))))

(define-syntax handle-tagbody
  (syntax-rules ()
    ((_ start (x ...) (a l ...) (b . u) . v)
     (handle-tagbody start (x ... (a (lambda () l ... (goto b)))) (b . u) . v))

    ((_ start (x ...) (a . l))
     (with-coroutines (x ...  (a (lambda () . l)))
       (start)))))
     
(define-syntax tagbody 
  (syntax-rules ()
    ((_ (a-label c ...) . l)
     (handle-tagbody a-label () (a-label c ...) . l))
    ((_) (if #f #f))))
