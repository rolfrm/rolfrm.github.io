(lambda (x)
  (println 'testing...)

  (let ((a (mat4:identity))
		  (b (mat4:identity))
		  (r (mat4:identity)))
	 (dotimes (i 1000)
		(mat4:multiplyi r a b)
		)
	 (println r)
	 
	 (then (loadWat
			  (println
				(value->string `
				 (module
				  (func $add (param $a i32) (result i32)
						  (local.get $a)
						  (i32.const 5)
						  (i32.add))
				  (func $m4mul (param $m1 i32) (param $m2 i32) (param $result i32)

						  ,@(let ((lst (list))
									 (offset-symbol (lambda (offset)
															(string->symbol (concat "offset=" (value->string (* 4 offset))))))
									 (local-ref (lambda (row column p) (string->symbol (concat "$m" p "r" row "c" column))))
									 )
								(dotimes! (row 4)
											 (dotimes! (column 4)
														  (push lst `(local ,(local-ref row column 0) f32))
														  ))
								(dotimes! (row 4)
											 (dotimes! (column 4)
														  (push lst `(local ,(local-ref row column 1) f32))
														  ))
								
								(dotimes! (row 4)
											 (dotimes! (column 4)
														  (set lst
																 (concat lst `((local.get $m1) (f32.load ,(offset-symbol (+ (* row 4) column))) (local.set ,(local-ref row column 0)))))
														  ))
								(dotimes! (row 4)
											 (dotimes! (column 4)
														  (set lst
																 (concat lst `((local.get $m2) (f32.load ,(offset-symbol (+ (* row 4) column))) (local.set ,(local-ref row column 1)))))
														  ))
										
								(dotimes! (row 4)
								  (dotimes! (column 4)
												(set lst (concat lst
												 
												 `((local.get $result)

													(local.get ,(local-ref row 0 0))
													(local.get ,(local-ref 0 column 1))
													(f32.mul)
													
													(local.get ,(local-ref row 1 0))
													(local.get ,(local-ref 1 column 1))
													(f32.mul)

													(local.get ,(local-ref row 2 0))
													(local.get ,(local-ref 2 column 1))
													(f32.mul)

													(local.get ,(local-ref row 3 0))
													(local.get ,(local-ref 3 column 1))
													(f32.mul)
													(f32.add)
													(f32.add)
													(f32.add)
													
													(f32.store ,(offset-symbol (+ column (* row 4))))
													  
								 )))))
								lst
								)
						  
						  )
			 
							
												 
				  (export "add" (func $add))
				  (export "m4mul" (func $m4mul))
				  (export "memory" (memory 0))
				  (memory 1)
))))
			 (lambda (module)
				;(println 'finished-loading (module.exports.add 52))
				(println module.exports.memory.buffer.byteLength)

				(let ((memory module.exports.memory)
					  (membuf (%js "new Float32Array(memory.buffer)"))
						(saved-matrixes (list))
						(finalizematrix (lambda (ptr) (push saved-matrixes ptr)))
						
						(finalizer (%js "new FinalizationRegistry(finalizematrix)"))
					  (a (mat4:identity))
					  (b (mat4:identity))
						(result (mat4:new))
						(mul module.exports.m4mul)
						(array-counter 0))
				  
				  (membuf.set a 0)
				  (membuf.set b 16)
				  
				  (dotimes (i 80)
					 
					 (mul 0 (* 4 16) (* 4 32) ))
				  (println (membuf.subarray 0 48))
				  
				  )
				
				
				)
	 )
	 )))
