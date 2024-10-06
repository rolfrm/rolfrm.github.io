(defun jsname (object)
  object.jsname)

(defun lisp2:compile-lisp (code resultVar)
  (let ((assign
			(cond
			  ((list? code)
				(case (car code)
				  ('let "unsupported")
				  (otherwise (concat (jsname (car code)) "(" (map lisp2:compile-lisp (cdr code)) ")"))
				  )
				
				)
			  (t (value->string code)))))
	 (println 'assign assign (type-of assign) resultVar)
	 (if (eq "return" resultVar)
		  (concat "return " assign)
		  (concat resultVar " = " assign))

  ))

(defun lisp2:compile-function(code)
  (println 'compile: code)
  (concat "() => {let tmp = null; " (lisp2:compile-lisp code "return") "}"))



