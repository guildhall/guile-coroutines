(define-module (stis coroutine)
  #:use-module (compat cl symbol-property)
  #:use-module (ice-9 match)
  #:use-module (syntax parse)
  #:use-module (srfi srfi-1)
  #:export (with-coroutines-raw 
	    with-coroutines
	    
	    goto
	    goto-from
	    
	    gosub
	    gosub-from

	    gosub-s
	    gosub-from-s

	    return
	    return-from

	    return-s
	    return-from-s

	    resume
	    resume-from
	    
	    resume-s
	    resume-from-s
	    
	    yield
	    yield-from

	    leave
	    leave-from

	    continue
	    continue-from

	    continue-sub
	    continue-sub-from

	    continue-sub-s
	    continue-sub-from-s

	    consume 	   
	    consume-from

	    transit
	    transit-from

	    feed
	    with-feed
	    with-return-stack

	    define-coroutine
	    define-coroutine-inited
	    
	    tagbody))

(define (union x y)
  (lset-union equal? x y))

(define-syntax-parameter RET
  (lambda (x) 
    (error 
     "TAG should be used in the transition part of with-coroutines")))

(define-syntax-rule (with-ret code ...)
  (let ((return-stack '()))
    (syntax-parameterize ((RET (make-variable-transformer
				  (lambda (x)
					  (syntax-case x (set!)
					    ((set! _ v) #'(set! return-stack v))
					    ((_ . _) 
					     (error "THUNK is not a funcition"))
					    (_ #'return-stack))))))
       code ...)))

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

(define ref-g  #f)
(define ref-gg #f)
(define-syntax-rule (with-g ((g tag) ...) code ...)
  (let-syntax ((g (lambda (x)
		    (syntax-case x (ref-g ref-gg)
		      ((_ ref-g abort kind . l)
		       #'(REGISTER-EXIT abort tag kind g . l))
		      ((_ ref-gg g abort kind . l)
		       #'(REGISTER-EXIT abort tag kind g . l))
		      (x (identifier? #'x) 
			 #'g)))) ...)
    code ...))
 
(define-syntax-class thk  
  (pattern ((~literal lambda) (a:id ...) code ...)
      #:with args  #'(a ...)
      #:with apply #'(list a ...))
  (pattern _
      #:with args  (datum->syntax #'1 (gensym "arg"))
      #:with apply #'args))

(define-syntax with-coroutines-raw
  (lambda (x)
    (syntax-parse x
      ((_ ((nm thunk:thk
	       ((exit (pat . transition-code) ...) ...))
	   ...)
	  code ...)
       #`(letrec ((nm  (let* ((tag (make-prompt-tag))
			      (th    
			       (with-tag tag 
				 (with-g #,(map (lambda (nm) #`(#,nm tag))
						#'(nm ...))
				    thunk)))
			      (init th))
			 (lambda thunk.args
			   (call-with-prompt tag
			       (lambda () (apply th thunk.apply))
			       (lambda (k q . args)
				 (exit-cond 
				  ((eq? q exit) 
				   (with (init th k)
					 (match args 
						(pat . transition-code) ...)))
				  ...))))))
		  ...)
	   code ...)))))


(define-syntax mk-exit
  (lambda (x)
    (syntax-case x ()
      ((_ nm)
       (let ((v (gensym (format #f "~a-exit" (syntax->datum #'nm)))))
	 (with-syntax ((s (datum->syntax #'1 v)))
	    #'(define-syntax nm (lambda (x) #''s))))))))

(mk-exit goto-exit)
(mk-exit gosub-exit)
(mk-exit return-exit)
(mk-exit return-s-exit)
(mk-exit resume-exit)
(mk-exit resume-s-exit)
(mk-exit yield-exit)
(mk-exit leave-exit)
(mk-exit continue-exit)
(mk-exit continue-sub-exit)
(mk-exit consume-exit)
(mk-exit transit-exit)
(mk-exit gosub-s-exit)
(mk-exit gosub-s-exit)
(mk-exit continue-sub-s-exit)

(define-syntax-rule (REGISTER-EXIT abort tag kind l ...)
  (let-syntax ((f
		(lambda (x)
		  (let ((r (get tag kind)))
		    (put tag 'seen (union (get tag 'seen '()) (list kind)))
		    (if r
			(put tag kind (max (length (list 'l ...)) r))
			(put tag kind (length (list 'l ...)))))
		  #f)))
	      
    f (abort tag kind l ...)))

(define-syntax exit-cond
  (lambda (x)
    (syntax-case x ()
      ((_ ((eq? u s) . l) ...)
       (with-syntax (((row ...) (generate-temporaries #'(s ...))))
	 #'(let-syntax ((row (lambda (x) 
			       (syntax-case x ()
				 ((_ r rs (... ...))
				  (if (get TAG s)
				      (if (= (length (get TAG 'seen)) 1)
					  (if (get TAG goto-exit) 
					      (syntax-case #'l (with match)
						(((with _ (match x (p n v))))
						 #'(match x (p v))))
					      #'(begin . l))
					  #'(if (eq? u s)
						(begin . l)
						(r rs (... ...))))
				      #'(r rs (... ...))))
				 ((_)
				  (if (get TAG s)
				      #'(if (eq? u s) 
					    (begin . l))
				      #'(if #f #f))))))
			...)
	     (row ...)))))))
     
;; GOTO
(define-syntax-rule (goto g . l)      
  (g ref-g abort-to-prompt goto-exit . l))
(define-syntax-rule (goto-from h g . l)      
  (h ref-gg g abort-to-prompt goto-exit . l))

(define-syntax-rule (continue g . l)  
  (g ref-g abort-to-prompt continue-exit . l))
(define-syntax-rule (continue-from h g . l)  
  (h ref-gg g abort-to-prompt continue-exit . l))

;; GOSUB
(define-syntax-rule (gosub g . l)     
  (g ref-g abort-to-prompt gosub-exit . l))
(define-syntax-rule (gosub-from h g . l)     
  (h ref-gg g abort-to-prompt gosub-exit . l))

(define-syntax-rule (gosub-s (s) g . l)     
  (g ref-g abort-to-prompt gosub-s-exit s . l))
(define-syntax-rule (gosub-from-s (s) h g . l)     
  (h ref-gg g abort-to-prompt gosub-s-exit s . l))

(define-syntax-rule (continue-sub g . l)  
  (g ref-g abort-to-prompt continue-sub-exit . l))
(define-syntax-rule (continue-sub-from h g . l)  
  (h ref-gg g abort-to-prompt continue-sub-exit . l))

(define-syntax-rule (continue-sub-s (s) g . l)  
  (g ref-g abort-to-prompt continue-sub-s-exit s . l))
(define-syntax-rule (continue-sub-from-s h (s) g . l)  
  (h ref-gg g abort-to-prompt continue-sub-s-exit s . l))

;; RETURN
(define-syntax-rule (return . l)  
  (REGISTER-EXIT abort-to-prompt TAG return-exit . l))
(define-syntax-rule (return-from g . l)  
  (g ref-g abort-to-prompt TAG return-from-exit . l))

(define-syntax-rule (resume      . l)  
  (REGISTER-EXIT abort-to-prompt TAG resume-exit . l))
(define-syntax-rule (resume-from g . l)  
  (g ref-g abort-to-prompt resume-exit . l))

(define-syntax-rule (return-s (s) . l)  
  (REGISTER-EXIT abort-to-prompt TAG return-s-exit s . l))
(define-syntax-rule (return-from-s (s) g . l)  
  (g ref-g abort-to-prompt TAG return-from-s-exit s . l))

(define-syntax-rule (resume-s (s) . l)  
  (REGISTER-EXIT abort-to-prompt TAG resume-s-exit s . l))
(define-syntax-rule (resume-from-s (s) g . l)  
  (g ref-g abort-to-prompt resume-s-exit s . l))

;; YIELD
(define-syntax-rule (yield x ...) 
  (REGISTER-EXIT abort-to-prompt TAG yield-exit x ...))
(define-syntax-rule (yield-from g x ...) 
  (g ref-g abort-to-prompt yield-exit x ...))

(define-syntax-rule (leave x ...) 
  (REGISTER-EXIT abort-to-prompt TAG leave-exit x ...))
(define-syntax-rule (leave-from g x ...) 
  (g ref-g abort-to-prompt leave-exit x ...))

;; CONSUME
(define-syntax consume 
  (syntax-rules ()
    ((_ v)   (begin
	       (when (not (pair? v)) 
		     (set! v (REGISTER-EXIT abort-to-prompt TAG consume-exit)))
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

;; TRANSIT
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
       (set! v (append v (REGISTER-EXIT 
			  abort-to-prompt TAG transit-exit data ...)))
       (let ((r (car v)))
	 (set! v (cdr v))
	 r)))
    ((() data ...)
     (abort-to-prompt TAG transit-exit data ...))))

(define-syntax-rule (with-coroutines ((nm thunk) ...) code ...)
  (with-ret
  (with-coroutines-raw ((nm thunk
			    ((goto-exit  
			      ((g . l) (coroutine-reset) (apply g l)))
			     
			     (gosub-exit
			      ((g . l)
			       (coroutine-reset)
			       (set! RET
				     (cons
				      (lambda x 
					(set! THUNK K)
					(apply nm x)) 
				      RET))
			       (apply g l)))

			     (gosub-s-exit
			      ((g RET . l)
			       (coroutine-reset)
			       (set! RET
				     (cons
				      (lambda x 
					(set! THUNK K)
					(apply nm x)) 
				      RET))
			       (apply g l)))
			     
			     (return-exit
			      (x 
			       (coroutine-reset)
			       (let ((gs RET))
				 (if (pair? gs)
				     (let ((g (car gs)))
				       (set! RET (cdr gs))
				       (apply g x))
				     (error "return-stack is empty")))))

			     (resume-exit
			      (x 
			       (coroutine-cont)
			       (let ((gs RET))
				 (if (pair? gs)
				     (let ((g (car gs)))
				       (set! RET (cdr gs))
				       (apply g x))
				     (error "return-stack is empty")))))

			     (resume-s-exit
			      ((RET . x)
			       (coroutine-cont)
			       (let ((gs RET))
				 (if (pair? gs)
				     (let ((g (car gs)))
				       (set! RET (cdr gs))
				       (apply g x))
				     (error "return-stack is empty")))))

			     (return-exit
			      (x 
			       (coroutine-cont)
			       (let ((gs RET))
				 (if (pair? gs)
				     (let ((g (car gs)))
				       (set! RET (cdr gs))
				       (apply g x))
				     (error "return-stack is empty")))))

			     (return-s-exit
			      ((RET . x)
			       (coroutine-cont)
			       (let ((gs RET))
				 (if (pair? gs)
				     (let ((g (car gs)))
				       (set! RET (cdr gs))
				       (apply g x))
				     (error "return-stack is empty")))))

			     (yield-exit 
			      (x   (coroutine-cont) (apply values x)))

			     (leave-exit 
			      (x   (coroutine-reset) (apply values x)))
			     
			     (continue-exit
			      ((g . l) (coroutine-cont) (apply g  l)))

			     (continue-sub-exit
			      ((g . l)
			       (coroutine-cont)
			       (set! RET
				     (cons
				      (lambda x 
					(set! THUNK K)
					(apply nm x)) 
				      RET))
			       (apply g l)))

			     (continue-sub-s-exit
			      ((g RET . l)
			       (coroutine-cont)
			       (set! RET
				     (cons
				      (lambda x 
					(set! THUNK K)
					(apply nm x)) 
				      RET))
			       (apply g l)))

			     (consume-exit
			      (()  (coroutine-cont) (if #f #f)))
			     
			     (transit-exit
			      (x   (coroutine-cont) (apply values x)))))
			...)
       code ...)))

(define-syntax-rule (feed g x ...) (g (list x ...)))
(define-syntax-rule (with-feed (q) code ...) (let ((q '())) code ...))
(define-syntax-rule (with-return-stack (q) code ...) (let ((q '())) code ...))

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
     (handle-tagbody start (x ... (a (lambda () 
				       (if #f #f) l ... (goto b)))) 
		     (b . u) . v))

    ((_ start (x ...) (a . l))
     (with-coroutines (x ...  (a (lambda () (if #f #f) . l)))
       (start)))))
     
(define-syntax tagbody 
  (syntax-rules ()
    ((_ (a-label c ...) . l)
     (handle-tagbody a-label () (a-label c ...) . l))
    ((_) (if #f #f))))
