(use-modules (stis coroutine))
(use-modules (ice-9 match))
		    
(define-coroutine (from n)
  (let loop ((i n))
    (yield i)
    (loop (+ i 1))))

(define-coroutine (in-list l)
  (let loop ((li l))
    (if (pair? li)
	(begin
	  (yield #t (car li))
	  (loop  (cdr li)))
	#f)))

(define-coroutine-inited (collector l)
  (with-feed (q)
    (let loop ((li l))
      (if (consume q)
	  (loop (cons (consume q) li))
	  (reverse li)))))

(let ((collect (collector '(start)))
      (numbers (from 10))
      (items   (in-list '(a b c d e f g h i j k l m n o p))))
  (let loop ()
    (call-with-values items
      (lambda (p . x)
	(if p
	    (begin
	      (feed collect #t (car x))
	      (loop))
	    (pk (feed collect #f)))))))

;; Plain old common lisp tagbody
(tagbody
 (a:
  (pk 'at-a))

 (b:
  (pk 'at-b))

 (c:
  (pk 'at-c))

 (d:
  (pk 'leave-at-d)))

(tagbody
 (a:
  (pk 'at-a)
  (goto c:))

 (b:
  (pk 'at-b)
  (goto d:))

 (c:
  (pk 'at-c)
  (goto b:))

 (d:
  (pk 'leave-at-d)))

;; But spiced with a gosub! yeah basic here we come!
(tagbody
 (a:
  (pk 'at-a)
  (gosub c:)
  (pk 'at-a))

 (b:
  (pk 'at-b)
  (goto d:))

 (c:
  (pk 'at-c)
  (return))

 (d:
  (pk 'leave-at-d)))
     

