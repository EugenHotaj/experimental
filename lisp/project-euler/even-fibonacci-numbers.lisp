;;; Start total=2 since we're only accumulating the even values
;;; and skipping the first 2 fibonacci numbers.
(let ((total 2) (left 1) (right 2)) 
  (loop while (< right 4000000)  do
        (setf right (+ left right))
        (setf left (- right left))
        (if (= (mod right 2) 0) 
          (setf total (+ total right))))
  total)
