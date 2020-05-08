;;;; Following along with Practical Common Lisp book.

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

(do-primes (p 0 19)
 (format t "~d " p ))

(defmacro do-primes-2 ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))) 
       ((> ,var ,end)) 
     ,@body))

(do-primes-2 (p 0 19)
 (format t "~d " p ))

(setf *print-pretty* t)

(macroexpand-1 '(do-primes-2 (p 0 19) (format t "~d " p)))

(do-primes-2 (p 0 (random 100))
 (format t "~d " p ))

(defmacro do-primes-3 ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))) 
          (ending-value-name ,end))
         ((> ,var ending-value-name)) 
       ,@body)))

(do-primes-3 (p 0 (random 100))
 (format t "~d " p ))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes-4 ((var start end) &body body)
  (with-gensyms (ending-value-name) 
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))) 
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name)) 
       ,@body)))

(do-primes-4 (p 0 (random 100))
 (format t "~d " p ))









