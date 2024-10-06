(then (loadFileAsync "c/math.wasm")
		(lambda (bytes)

		  (then
			(WebAssembly.instantiate bytes (makehashmap))
			(lambda (module)
			  (let ((mul module.instance.exports.mat4_multiply)
					  (mat4_copy module.instance.exports.mat4_copy)
					  (memory module.instance.exports.memory)
					  (membuf (%js "new Float32Array(memory.buffer)"))
					  (a (mat4:identity))
					  (b (mat4:identity))
					  (result (mat4:new))
					  (saved-matrixes (list))
					  (array-counter 0)
					  (finalize (lambda (id) (push saved-matrixes id)))
					  (finalizer (%js "new FinalizationRegistry(finalize)"))
					  )
				 (println memory)

				 (let ((create-matrix (lambda ()
												 (when (length saved-matrixes)
													(println 'take-saved! (car saved-matrixes)))
												(let ((matrix (list 'matrix (if (length saved-matrixes)
																							  (pop saved-matrixes) (incf array-counter (* 4 16))) (list))))
						(finalizer.register matrix array-counter (caddr matrix))
						
						matrix
						)))
						 (copy (lambda (dst src) (mat4_copy (cadr dst) (cadr src))))
						 (multiply (lambda (a b r)
										 (mul (cadr r) (cadr a) (cadr b) )))
						 (transfer (lambda (a r)
										 (membuf.set a (/ (cadr r) 4))))
						 (read-back (lambda (a)
										  (membuf.subarray (/ (cadr a) 4) (/ (+ (cadr a) 16) 4))))
						 (delete (lambda (a)
									  (finalizematrix (cadr a))
									  (finalizer.unregister (caddr a))
									  ))

						 )
					(let ((m1 (create-matrix))
							(a (mat4:translation 1 0 0))
							(m2 (create-matrix))
							(b (mat4:translation -10 0 0))
							(m3 (create-matrix))
							)
					  (transfer a m1)
					  (transfer b m2)
					  (println m1 m2 m3)
					  ;(println (read-back m1) m1 m2 m3)
					  (dotimes (i 10);70000000)
						 (multiply m2 m1 m3)
						 (copy m2 m3)
						 )
					  (println (membuf.subarray 0 64))
					  (println (make-object))
					  )))))))