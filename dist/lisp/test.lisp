(loadfile "lisp/lisp.lisp")

(println 123)
(defun asd (a b c) (+ a b c))
(println (concat (list 1 2 3) (list 4 5 6)))
(println (asd 1 2 3))
(println (let ((a 10)) (%js "a + 1 + 2 + 3 + 4")))
;(println (case-code (cdr '(case 3 (3 b) (4 d) (5 e) (6 g)))))
(println (link-ends '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
(println "gets here")
(println (case 10 (9 (+ 1 2)) (10 (+ 3 4))))

(println `(1 2 3 "asd" asd (3 2 1 ,(+ 1 2) ,@(list 4 5 6) 1 2 "hello
world")))


(assert-eq 1234 (when 1 1234))
(assert-eq 1234 (unless 0 1234))
(assert-not-eq 0 (when 1 1234))

(assert-eq "function" (type-of (lambda (x) (+ x 2))))
(assert-eq 3 (progn 1 2 3))
(assert-eq nil (progn))
(assert-eq 0.25 (/ 4.0))
(assert-eq 2 (/ 24 4 3))
(assert-eq 1 (/ 24 4 3 2))
(assert-eq 25 (/ -100 -4))
(assert-eq 24 (* 4 3 2))
(assert-eq 1 (* 1))
(assert-eq 2 (* 1 2))
(assert-eq 6 (* 1 2 3))
(assert-eq 1 (*))
(assert-eq 0 (+))

(println (lambda (x y &rest args) (list x y args)))

(assert-not-equals '(1 2 3) '(0 1 2 3))
(assert-equals '(1 2 3) '(1 2 3))
(assert-not-eq '(1 2 3) '(2 3 4))

(defvar own-keys (%js "Reflect.ownKeys"))

(let  ((console-log (%js "console.log")))
  (console.log (list "123" 111 222 'asd))

  )
(let ((sum 0))
  (for-each x '(1 2 3)
				(set sum (+ x sum)))
  (assert-eq sum 6))

(assert (undefined? asdasdasdasdasd))
(let ((xyz 1))
  (assert-not (undefined? xyz)))

(let ((a 1))
  (incf a 3)
  (assert-eq a 4)
  (incf a)
  (assert-eq a 5)
  (decf a)
  (assert-eq a 4)
  (decf a 3)
  (assert-eq a 1)
  )

(defun make-sphere (center radius)
  (lambda (x y z) 

  ))

(let ((sphere '(0.0 0.0 0.0 1.0))
		(ray (list 0 0 0)))
  
  
  )

(let ((tmp 123))
  (assert-eq tmp 123))

(println (type-of nil) ">" nil)
(println (type-of (list 1 2 3)))
(assert (list? (list 1 2 3)))
(assert-not (list? nil))

(defvar not-list-items (list 1 "asd" nil (lambda () 2) (make-map)))
(defvar list-items (list (list 1 2 3) (list) (list "123")))
(defvar string-items (list "" "123" "123123123123"))
(defvar not-string-items (list 1 (list 1) nil))
(defvar symbol-items (list 'a 'asd123 (quote hej)))
(defvar not-symbol-items (list 1 "asd" (list 123)))
(for-each x not-list-items (assert-not (list? x)))
(for-each x list-items (assert (list? x)))
(println (type-of (car string-items)))
(for-each x string-items
			 (assert (string? x) "not a string item!" x))
(for-each x not-string-items (assert-not (string? x)))
(for-each x symbol-items (assert (symbol? x) "not a string item!"))
(for-each x not-symbol-items (assert-not (symbol? x) "was a string item!"))

(assert (number? 5))
(assert-not (number? "5"))
(assert-not (nth (list 1) 1))
(assert-equals (cdr "123") (cdr "323"))
(assert-equals (cddr "123") (cddr "323"))
(assert-eq (cdddr "12345") "45")
(assert-eq nil (cdddr "1"))
(assert-not (cdddr (list 1)))
(assert-eq 0 (len nil))


(defvar  test-declare (lambda (x)
										 (declare (type string x))
										 x))

(handle-errors (raise "x") (y (println 'error-caught)))

(test-declare "123")
(handle-errors (progn (test-declare 123) (raise 'failed))
					(error (println ">>" error)
							 (assert-not (eq error 'failed))))

(let ((r (block asd (handle-errors (return-from asd 123) (e 333)))))
  (assert (eq r 123)))
(let ((r (handle-errors (block asd (raise "oh no") (return-from asd 5))
								(e (progn (assert (eq e "oh no") ) 111)))))
  (assert (eq r 111)))

(let ((x 0) (end 10000000))
  (loop (< x end)
	(incf x 1)
	))

(assert-eq nil (cdr nil))
(assert-eq nil (cddr nil))
(assert-eq nil (car nil))
(assert-eq nil (cadr nil))
(println (car nil))

(defvar str0 "asd
asd
asd ")
(assert-eq 12 (length str0))

(println '> '((1 2) (w d) (4 ( 4 5))) '< )

(assert (ismacro 'defun))
(assert-not (ismacro 'defunasdasd))

(let ((n2 (order-by (list -1 5 4 3 2 1) (lambda (x) x))))
  (assert-eq -1 (getnth n2 0))
  (assert-eq 1 (getnth n2 1))
  (assert-eq 2 (getnth n2 2))
  (println n2)
)
(let ((test-list (list 1 2 3)))
   (let ((plist (select test-list (lambda (x) (- x)))))
       (assert-eq -1 (getnth plist 0))
       (assert-eq -2 (getnth plist 1))
   )
)

(let ((test (lambda (value)
        (case value
          (1 'a)
          (2 'b)
          (otherwise 'c)
  ))))
  (assert-eq 'c (test -1))
  (assert-eq 'a (test 1))
  (assert-eq 'b (test 2))
  (assert-eq 'c (test 0))
  (assert-eq 'c (test 5))
)

(assert-eq 20 (aggregate (select '(1 2 3 4) (lambda (x) (* 2 x))) (lambda (x y) (+ x y))))
(assert-equals '(4 5) (where '(1 2 3 4 5) (lambda (x) (> x 3))))
(assert-equals '(1 2 3) (take '(1 2 3 4 5) 3))
(assert-equals '(4 5) (skip '(1 2 3 4 5) 3))
(println Infinity)


(println (apply println '(1 2 3 4)))

(defun test-server ()
(let ((server (net.createServer(lambda (c)
  (println "client connected!")
     (c.write "hello!
  ")
  (c.pipe c)
))))
  (async-call (lambda () (server.listen 11111)))
  (println 'ok-server-started)
  (async-call (lambda () 
     (let ((client (net.createConnection (%js "{port: 11111}") 
       (lambda () (println "connected to server")))
  ))
        (client.on "data" (lambda(d) (println 'got-data: d) (client.end)
           (server.close)
        
        ))))
  
  )
))
(defun hello-func-def (a b) (println a b))

(defvar hello-func-2 (let ((x 0)) (lambda (a) (incf x a))))
(println (hello-func-2 2))
(println (hello-func-2 2))

(assert-eq "(1 2 3 (asd dsa))" (value->string '(1 2 3 (asd dsa))))
(assert-eq 'abc123 (string->symbol "abc123"))

(defun cond-test (x)
   (cond 
      ((string? x) "string")
      ((number? x) "number")
      (t "unknown")
   )

)
(assert-eq (cond-test "asd") "string")
(assert-eq (cond-test 123) "number")
(assert-eq (cond-test 'hej) "unknown")

(assert-equals '(3 4 5) (memq 3 '(0 1 2 3 4 5)))
(assert-equals 33 (elt (list 11 22 33 44) 2))
(assert-equals 2 (abs -2))
(assert-equals 2 (min 6 3 8 2 5 3))
(assert-equals 8 (max 6 -3 8 2 5 3))
(assert-equals 2 (clamp 2 -1 3))
(assert-equals 3 (clamp 5 -1 3))
(assert-equals -1 (clamp -2 -1 3))

(assert-eq 2 (floor 2.5))


(println (function-signature (eval 'memq)))
(println 'all-done)
(println loadcontext)

(load "load_test.lisp")

;; test hash map
(let ((hmap (makehashmap)))
   (hashmap-set hmap "123" 1)
   (hashmap-set hmap "4" 2)
   (assert-eq 1 (hashmap-get hmap "123"))
   (assert-eq 2 (hashmap-get hmap "4"))
   (hashmap-delete hmap "123")
   (assert (not (undefined? (hashmap-get hmap "4"))))
   (assert (undefined? (hashmap-get hmap "123")))
   (hashmap-set hmap "4" undefined)
   (println (hashmap-keys hmap))
)