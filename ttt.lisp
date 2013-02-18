;;(setf *tbd* '(nil nil nil nil nil nil nil nil nil))

;; create list of 9 elements
(setf *tbd* (make-list 9))

;; check if board is at winning position for x
(defun won? (x)
  (or (and
	(eq (nth 0 *tbd*) x)
	(eq (nth 1 *tbd*) x)
	(eq (nth 2 *tbd*) x))
      (and
	(eq (nth 3 *tbd*) x)
	(eq (nth 4 *tbd*) x)
	(eq (nth 5 *tbd*) x))
      (and
	(eq (nth 6 *tbd*) x)
	(eq (nth 7 *tbd*) x)
	(eq (nth 8 *tbd*) x))
      (and
	(eq (nth 0 *tbd*) x)
	(eq (nth 3 *tbd*) x)
	(eq (nth 6 *tbd*) x))
      (and
	(eq (nth 1 *tbd*) x)
	(eq (nth 4 *tbd*) x)
	(eq (nth 7 *tbd*) x))
      (and
	(eq (nth 2 *tbd*) x)
	(eq (nth 5 *tbd*) x)
	(eq (nth 8 *tbd*) x))
      (and
	(eq (nth 0 *tbd*) x)
	(eq (nth 4 *tbd*) x)
	(eq (nth 8 *tbd*) x))
      (and
	(eq (nth 2 *tbd*) x)
	(eq (nth 4 *tbd*) x)
	(eq (nth 6 *tbd*) x))))

;; check if 'x' can win if entered at 'n'th position
(defun isWinPos? (x n)
	(setf (nth n *tbd*) x)
	(cond ((eq (won? x) t) t)
	      (t (setf (nth n *tbd*) nil))))

;; check position
(defun checkPos? (x n)
  (cond ((eq (nth n *tbd*) nil) (isWinPos? x n))
	(t nil)))
	 
	     
;; Run loop to check each nil position for winning

(defun runLoop (x)
  (let ((i 0))
    (loop while (AND (eq (checkPos? x i) nil) (< i 8)) do
	  (incf i))
    (cond ((< i 8) i)
	  (t nil))))


;; print the tictactoe table   
(defun printAll(x)
  (loop for i in '(0 1 2 3 4 5 6 7 8) do
	(format t "~a " (if (eq (nth i x) 9)
			    "x"
			  (if (eq (nth i x) 0)
			      "o"
			    nil)))
	(cond ((eq (mod (+ i 1) 3) 0) (format t "~%"))
	      (t nil)))
  (format t "~%"))

;;read user input
(defun userInput (x)
  (let ((i (read)))
	(setf (nth i *tbd*) x)))

;; Randomly place the number
(defun playRandom (x)
  (setf i (random 8))
  (loop while (not (eq (nth i *tbd*) nil)) do
	(setq i (random 8)))
  (setf (nth i *tbd*) x))


;; get opposite player
(defun getOpp (x)
  (cond ((eq 0 x) 9)
	(t 0)))

(defun playnext (x)
  (if (runLoop  x)
      t
    (let ((y (runLoop (getopp x))))
      (if (eq y nil)
	  (playRandom x)
	(progn (setf (nth y *tbd*) nil)
	       (setf (nth y *tbd*) x))))))



;; this is main loop
;; 9 is you
;; 0 is computer :)
(defun game ()
(loop while (not (or (won? 9) (won? 0))) do
      (progn (userInput 9)
	     (printAll *tbd*)
	     (playnext 0)
	     (printAll *tbd*))))