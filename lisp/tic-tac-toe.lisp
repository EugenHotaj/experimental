;;; Simple terminal Tic Tac Toe game in (bad) Lisp.
;;; 
;;; Rules:
;;; Players take turns with X going first then alternating.
;;; Each player specifies a next move by the following commands:
;;;   tl=top left      cl=center left     tr=top right
;;;   cl=center left   cc=center          cr=center right
;;;   bl=bottom left   bc=bottom center   br=bottom right

;; All the available moves.
(defparameter *moves* (make-hash-table))
(setf (gethash 'tl *moves*) '(0 0))
(setf (gethash 'tc *moves*) '(0 1))
(setf (gethash 'tr *moves*) '(0 2))
(setf (gethash 'cl *moves*) '(1 0))
(setf (gethash 'cc *moves*) '(1 1))
(setf (gethash 'cr *moves*) '(1 2))
(setf (gethash 'bl *moves*) '(2 0))
(setf (gethash 'bc *moves*) '(2 1))
(setf (gethash 'br *moves*) '(2 2))

;; All the combinations which will lead to a victory.
(defparameter *winning-combos* 
  (list (list 'tl 'tc 'tr)   ; top horizontal 
        (list 'cl 'cc 'cr)   ; middle horizontal 
        (list 'bl 'bc 'br)   ; bottom horizontal 
        (list 'tl 'cl 'bl)   ; left vertical
        (list 'tc 'cc 'bc)   ; center vertical
        (list 'tr 'cr 'br)   ; right vertical
        (list 'tl 'cc 'br)   ; diagonal
        (list 'tr 'cc 'bl))) ; diagonal

(defun reinit-board ()
  "Clears the board."
  (defvar *board* nil)
  (setf *board* (make-array '(3 3)
                              :initial-contents
                              '((nil nil nil)
                                (nil nil nil)
                                (nil nil nil)))))

(defun print-board ()
  """Prints the elements of the board."""
  (loop for i below (first (array-dimensions *board*)) do
    (loop for j below (second (array-dimensions *board*)) do
      (let ((cell (aref *board* i j)))
        (format t "| ~a |" (if cell cell " "))))
        (if (< i 2) (format t "~%|---||---||---|~%"))))

(defun check-victory (player)
  "Returns T if the player has any of the winnig-combos."
  (loop for (one two three) in *winning-combos* do
    (let ((idx-one (gethash one *moves*))
          (idx-two (gethash two *moves*))
          (idx-three (gethash three *moves*)))
      (if (and 
           (eql (aref *board* (first idx-one) (second idx-one)) player) 
           (eql (aref *board* (first idx-two) (second idx-two)) player)
           (eql (aref *board* (first idx-three) (second idx-three)) player))
        (return-from check-victory t))))
  nil)

(defun check-tie()
  "Checks to see if there is a tie."
  (loop for i below (first (array-dimensions *board*)) do
    (loop for j below (second (array-dimensions *board*)) do
      (if (not (aref *board* i j)) (return-from check-tie nil))))
  t)

(defun make-move (player move)
  "Marks the move of the player on the board."
  (let ((idx (gethash move *moves*)))
    (setf (aref *board* (first idx) (second idx)) player)))

