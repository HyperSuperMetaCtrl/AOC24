(import 'str:split)
(defparameter *double-newline* (format nil "~%~%"))
(defun split-input (input)
  (split *double-newline* (uiop:read-file-string input)))

(defun parse-input (delim input)
  (mapcar #'(lambda (x) (mapcar #'parse-integer (split delim x))) input))

(defun is-ordered (x y order)
  (not (member (list y x) order :test #'equal)))

(defun middle (lst)
  (nth (1- (ceiling (length lst) 2)) lst))

(defun is-sorted (lst order)
  (every #'(lambda (x y) (is-ordered x y order)) lst (rest lst)))

(defun main ()
  (let* ((input (split-input "input"))
	 (rules (parse-input "|" (str:words (first input))))
	 (updates (parse-input "," (str:words (second input)))))
    (format t "Part 1: ~D~%" (reduce #'+ (remove nil (mapcar #'(lambda (x) (if (is-sorted x rules) (middle x))) updates))))
    (format t "Part 2: ~D~%" (reduce #'+ (remove nil (mapcar #'(lambda (x) (if (not (is-sorted x rules)) (middle (sort x #'(lambda (x y) (is-ordered x y rules)))))) updates))))
    ))
(main)


