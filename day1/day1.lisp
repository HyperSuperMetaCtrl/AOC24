(defparameter *input-path* "input")

(defun get-input ()
  (uiop:read-file-lines *input-path*))

(defun split-line (line)
  (mapcar #'parse-integer
	  (cl-utilities:split-sequence #\Space line :remove-empty-subseqs t)))

(defun parse-input ()
  (let ((lines (get-input)))
    (loop for line in lines
	  for (x y) = (split-line line)
	  collect x into xs
	  collect y into ys
	  finally (return (list xs ys)))))

(defun sort-lists (lists)
  (mapcar (lambda (lst) (sort lst #'<)) lists))

(defun calculate-differences (sorted-lists)
  (let ((list1 (first sorted-lists))
	(list2 (second sorted-lists)))
    (loop for x in list1
	  for y in list2
	  sum (abs (- x y)))))

(let* ((parsed-input (parse-input))
       (sorted-input (sort-lists parsed-input)))
  (format t "Sum of differences: ~A~%" (calculate-differences sorted-input)))
