
In my efforts for creating effective 3D math in Lisp/javascript and came up with an interesting macro, `mat4:multiply!`.

Since the regular 3D transformations only affects a small subset of the element in the matrix, it is much faster to only work on those elements in-place rather than do full matrix multiplications.

In my code I often rotate around a specific axis and when this axis is known, it is also much faster to do a axis-based rotation than a full angle-vector rotation - of course you might want to rotate around a specific axis and this is also faster using this method.

Take for example rotate-x:

```lisp
(defun mat4:rotate-x (m rad)
  (const ((cosA (math:cos rad))
			 (sinA (math:sin rad))
			 (isinA (- 0 sinA)))
	 (mat4:multiply! m 
						  1 0 0 0
						  0 cosA sinA 0
						  0 isinA cosA 0
						  0 0 0 1)))
```

Notice that many of the elements of the matrix are either 1 or 0. In many transformations there are 1's in the diagonal and this translates roughly to a no-op. 0's does not translate to a no-op directly, but it does translate to an element not being used.


The multiply! macro checks the arguments for 0's and only accesses the right indexes in the matrix.

Generating the following javascript code:

```javascript

(m, rad) => {
    var lambdaResult;
    {
        const cosA = math_cos(rad);
        const sinA = math_sin(rad);
        const isinA = (0 - sinA);
        {
            const m4 = m[4];
            const m5 = m[5];
            const m6 = m[6];
            const m7 = m[7];
            const m8 = m[8];
            const m9 = m[9];
            const m10 = m[10];
            const m11 = m[11];
            m[4] = (m4 * cosA + m8 * sinA);
            m[5] = (m5 * cosA + m9 * sinA);
            m[6] = (m6 * cosA + m10 * sinA);
            m[7] = (m7 * cosA + m11 * sinA);
            m[8] = (m4 * isinA + m8 * cosA);
            m[9] = (m5 * isinA + m9 * cosA);
            m[10] = (m6 * isinA + m10 * cosA);
            m[11] = (m7 * isinA + m11 * cosA);
            lambdaResult = 0;
        };
    }
    return lambdaResult
}

```

A regular 4x4 matrix requires 4x4x4=64 multiplications. This optimized version only requires 16. The expected x4 performance was also observed in practice.

### Translation

Complexity: 3x4=12 multiplications (compared to 64)

```lisp

(defun mat4:translate (m x y z)
  (mat4:multiply! m
     1 0 0 0
     0 1 0 0
     0 0 1 0
     x y z 1))

```

```javascript

(m, x, y, z) => {
    var lambdaResult;
    {
        const m0 = m[0];
        const m1 = m[1];
        const m2 = m[2];
        const m3 = m[3];
        const m4 = m[4];
        const m5 = m[5];
        const m6 = m[6];
        const m7 = m[7];
        const m8 = m[8];
        const m9 = m[9];
        const m10 = m[10];
        const m11 = m[11];
        const m12 = m[12];
        const m13 = m[13];
        const m14 = m[14];
        const m15 = m[15];
        m[12] = (m0 * x + (m4 * y + (m8 * z + m12 * 1)));
        m[13] = (m1 * x + (m5 * y + (m9 * z + m13 * 1)));
        m[14] = (m2 * x + (m6 * y + (m10 * z + m14 * 1)));
        m[15] = (m3 * x + (m7 * y + (m11 * z + m15 * 1)));
        lambdaResult = 0;
    }
    return lambdaResult
}
```

### Scaling

Complexity: 3x12 multiplications

```lisp
(defun mat4:scale (m x y z)
  (mat4:multiply! m
						x 0 0 0 
						0 y 0 0 
						0 0 z 0 
						0 0 0 1))
```

```javascript
(m, x, y, z) => {
    var lambdaResult;
    {
        const m0 = m[0];
        const m1 = m[1];
        const m2 = m[2];
        const m3 = m[3];
        const m4 = m[4];
        const m5 = m[5];
        const m6 = m[6];
        const m7 = m[7];
        const m8 = m[8];
        const m9 = m[9];
        const m10 = m[10];
        const m11 = m[11];
        m[0] = m0 * x;
        m[1] = m1 * x;
        m[2] = m2 * x;
        m[3] = m3 * x;
        m[4] = m4 * y;
        m[5] = m5 * y;
        m[6] = m6 * y;
        m[7] = m7 * y;
        m[8] = m8 * z;
        m[9] = m9 * z;
        m[10] = m10 * z;
        m[11] = m11 * z;
        lambdaResult = 0;
    }
    return lambdaResult
}
```

### Arbitrary Axis Rotation

Complexity: ~12x4=48


```lisp

(defun mat4:rotate (m angle axis-vector)
  (const ((l (vec3:length-squared axis-vector)))
	 (if (not (eq l 1.0))
		  (set axis-vector (vec3:div-scalar (math:sqrt l)))))
  
  (const ((rad angle) 
        (cosA (math:cos rad))
        (sinA (math:sin rad))
        (icosA (- 1 cosA))
		  (x (vec3:x axis-vector))
		  (y (vec3:y axis-vector))
		  (z (vec3:z axis-vector))
		  )
	 (mat4:multiply! m
						  (+ cosA (* x x icosA))
						  (- (* x y icosA) (* z sinA))
						  (+ (* x z icosA) (* y sinA))
						  0
						
						  (+ (* y x icosA) (* z sinA))
						  (+ cosA (* y y icosA))
						  (- (* y z icosA) (* x sinA))
						  0
						  
						  (- (* z x icosA) (* y sinA))
						  (+ (* z y icosA) (* x sinA))
						  (+ cosA (* z z icosA))
						  0
						  0 0 0 1)))

```


```javascript
(m, angle, axis_vector) => {
    var lambdaResult;
    {
        const l = vec3_length_squared(axis_vector);
        (not(eq(l, 1)) ? axis_vector = vec3_div_scalar(math_sqrt(l)) : null);
    };
    {
        const rad = angle;
        const cosA = math_cos(rad);
        const sinA = math_sin(rad);
        const icosA = (1 - cosA);
        const x = vec3_x(axis_vector);
        const y = vec3_y(axis_vector);
        const z = vec3_z(axis_vector);
        {
            const m0 = m[0];
            const m1 = m[1];
            const m2 = m[2];
            const m3 = m[3];
            const m4 = m[4];
            const m5 = m[5];
            const m6 = m[6];
            const m7 = m[7];
            const m8 = m[8];
            const m9 = m[9];
            const m10 = m[10];
            const m11 = m[11];
            m[0] = (m0 * (cosA + x * x * icosA) + (m4 * (x * y * icosA - z * sinA) + m8 * (x * z * icosA + y * sinA)));
            m[1] = (m1 * (cosA + x * x * icosA) + (m5 * (x * y * icosA - z * sinA) + m9 * (x * z * icosA + y * sinA)));
            m[2] = (m2 * (cosA + x * x * icosA) + (m6 * (x * y * icosA - z * sinA) + m10 * (x * z * icosA + y * sinA)));
            m[3] = (m3 * (cosA + x * x * icosA) + (m7 * (x * y * icosA - z * sinA) + m11 * (x * z * icosA + y * sinA)));
            m[4] = (m0 * (y * x * icosA + z * sinA) + (m4 * (cosA + y * y * icosA) + m8 * (y * z * icosA - x * sinA)));
            m[5] = (m1 * (y * x * icosA + z * sinA) + (m5 * (cosA + y * y * icosA) + m9 * (y * z * icosA - x * sinA)));
            m[6] = (m2 * (y * x * icosA + z * sinA) + (m6 * (cosA + y * y * icosA) + m10 * (y * z * icosA - x * sinA)));
            m[7] = (m3 * (y * x * icosA + z * sinA) + (m7 * (cosA + y * y * icosA) + m11 * (y * z * icosA - x * sinA)));
            m[8] = (m0 * (z * x * icosA - y * sinA) + (m4 * (z * y * icosA + x * sinA) + m8 * (cosA + z * z * icosA)));
            m[9] = (m1 * (z * x * icosA - y * sinA) + (m5 * (z * y * icosA + x * sinA) + m9 * (cosA + z * z * icosA)));
            m[10] = (m2 * (z * x * icosA - y * sinA) + (m6 * (z * y * icosA + x * sinA) + m10 * (cosA + z * z * icosA)));
            m[11] = (m3 * (z * x * icosA - y * sinA) + (m7 * (z * y * icosA + x * sinA) + m11 * (cosA + z * z * icosA)));
            lambdaResult = 0;
        };
    };
    return lambdaResult
}

```