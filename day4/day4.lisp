(ql:quickload :select)
(ql:quickload :array-operations)

(defparameter *xmas* "XMAS")
(defparameter *xmas-len* (length *xmas*))
(defparameter *kernel-strings-vert* '(("XMAS") ("SAMX")))
(defparameter *kernel-strings-horiz* '(("X" "M" "A" "S") ("S" "A" "M" "X")))
(defparameter *kernel-strings-diag* '(("X..."
				       ".M.."
				       "..A."
				       "...S")
				      ("S..."
				       ".A.."
				       "..M."
				       "...X")
				      ("...X"
				       "..M."
				       ".A.."
				       "S...")
				      ("...S"
				       "..A."
				       ".M.."
				       "X...")))


(defun parse-input (input-path)
  (let ((input (uiop:read-file-lines input-path)))
    (loop for line in input
	  collect (map 'list #'char-int line))))

(defun convert-kernel-strings (kernel-strings)  
  (mapcar #'(lambda (kernel) (mapcar #'(lambda (str) (map 'list #'char-int str)) kernel)) kernel-strings))

(defun list-dim (lst)
  `(,(length lst) ,(length (car lst))))

(defun to-arrays (xdim ydim lst)
  (mapcar #'(lambda (x) (make-array (list xdim ydim) :initial-contents x)) lst))

(defun main ()
  (let* ((input (parse-input "input"))
	 (kernels-vert (to-arrays 1 *xmas-len* (convert-kernel-strings *kernel-strings-vert*)))
	 (kernels-horiz (to-arrays *xmas-len* 1 (convert-kernel-strings *kernel-strings-horiz*)))
	 (kernels-diag (to-arrays *xmas-len* *xmas-len* (convert-kernel-strings *kernel-strings-diag*)))
	 (counter 0)
	 (input-array (make-array (list-dim input) :initial-contents input)))

    (loop for i from 0 to (- (length input) *xmas-len*) do
      (loop for j from 0 below (length input) do
	(let ((vert (select:select input-array (select:range i (+ i *xmas-len*)) j)))
	  (when (every #'identity (aops:each #'= vert (select:select (first kernels-vert) 0 t))) (incf counter))
	  (when (every #'identity (aops:each #'= vert (select:select (second kernels-vert) 0 t))) (incf counter)))))

    (loop for i from 0 below (length input) do
      (loop for j from 0 to (- (length input) *xmas-len*) do
	(let ((horiz (select:select input-array i (select:range j (+ j *xmas-len*)))))
	  (when (every #'identity (aops:each #'= horiz (select:select (first kernels-horiz) t 0))) (incf counter))
	  (when (every #'identity (aops:each #'= horiz (select:select (second kernels-horiz) t 0))) (incf counter)))))

    (loop for i from 0 to (- (length input) *xmas-len*) do
      (loop for j from 0 to (- (length input) *xmas-len*) do
	(let ((diag (select:select input-array
				   (select:range i (+ i *xmas-len*))
				   (select:range j (+ j *xmas-len*)))))
	  (when (eq 4 (count t (aops:flatten (aops:each #'= diag (first kernels-diag))))) (incf counter))
	  (when (eq 4 (count t (aops:flatten (aops:each #'= diag (second kernels-diag))))) (incf counter))
	  (when (eq 4 (count t (aops:flatten (aops:each #'= diag (third kernels-diag))))) (incf counter))
	  (when (eq 4 (count t (aops:flatten (aops:each #'= diag (fourth kernels-diag))))) (incf counter)))))
    (format t "Day 4 Part 1: ~d~%" counter)))

  (main)







