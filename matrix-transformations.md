
In my efforts for creating effective 3D math in Lisp/javascript and came up with an interesting macro, `mat4:multiply!`.

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