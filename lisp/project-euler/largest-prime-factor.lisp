(defparameter *num* 600851475143)
(defparameter *sqrt-num* (ceiling (sqrt *num*)))

;;; Create the prime number sieve.
(defparameter *sieve*
  (loop for i from 2 to *sqrt-num* collect i))

(loop for current in *sieve*  do 
      (loop for iterator in *sieve* do
            (if (and (not (eq current iterator))
                    (eq (mod iterator current) 0))
              (setf *sieve* (remove iterator *sieve*)))))

*sieve*


