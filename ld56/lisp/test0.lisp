(loadfile "lisp/lisp.lisp")
(let ((a 1) (c 3) (a 2))
  (let ((b 2))
	 (println (op_add a (op_add b c)))))

(let ((a 0))
  (loop (_op_lt a 100)
		  (set a (op_add a 1))
		  (set a (op_add a 1))
		  (set a (op_add a 1))
		  (set a (op_add a 1))
		  (set a (op_add a 1))
		  (println a)))
(let ((f (_lambda (x y) (op_add x y))))
  (println (f 3 4)))

(let ((x 0))
  (set x (let ((y 3)) (op_add y x))))
(if (let ((x 1 )) x)
	 (let ((y 1233)) (println y))
	 (println 3))

(defvar  fib (lambda (x) (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2))))) )


(println (if 0 (let ((x 1)) x) 2))
(println (and 1 2 3 4))
(println (or 0 3 2 1))

(defvar xyz (let ((x 3 )(y 2)) (+ x y)))
(println xyz)
(defvar / op_div)

(println
 (_block return2
	(return-from return2 (let ((a 1)) a))))

(println (handle-errors
 (raise ":(")
 (v (list 'got ":("))))
(println (handle-errors
 ":)"
 (v (println v))))




(defmacro type-of(x)
  `(handle-errors
	 (_typeof ,x)
	 (e "undefined")))

(defun object? (item) (eq (type-of item) "object"))
(defun null? (item) (and (object? item) (not item)))

