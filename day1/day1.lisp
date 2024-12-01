(defparameter *input-path* "input")

(defun get-input ()
  "read file into lines"
  (uiop:read-file-lines *input-path*))

(defun split-line (line)
  "split lines into two lists and parse from string to integer"
  (mapcar #'parse-integer
	  (cl-utilities:split-sequence #\Space line :remove-empty-subseqs t)))

(defun parse-input ()
  "return the complete parsed input"
  (let ((lines (get-input)))
    (loop for line in lines
	  for (x y) = (split-line line)
	  collect x into xs
	  collect y into ys
	  finally (return (list xs ys)))))

(defun sort-lists (lists)
  "lists is a lists of lists and returns the lists sorted"
  (mapcar (lambda (lst) (sort lst #'<)) lists))

(defun calculate-differences (sorted-lists)
  "calculate the elementewise difference between two sorted lists"
  (let ((list1 (first sorted-lists))
	(list2 (second sorted-lists)))
    (loop for x in list1
	  for y in list2
	  sum (abs (- x y)))))

;; Day 1 Part 1
(let* ((parsed-input (parse-input))
       (sorted-input (sort-lists parsed-input)))
  (format t "Sum of differences: ~A~%" (calculate-differences sorted-input)))

;; Day 1 Part 2
(let* ((parsed-input (parse-input))
       (left-list (first parsed-input))
       (right-list (second parsed-input))
       (sum (loop for left in left-list
		  sum (* left (count left right-list)))))
  (format t "Sum of Products: ~A~%" sum))
