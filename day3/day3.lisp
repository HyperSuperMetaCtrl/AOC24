(ql:quickload "cl-ppcre")

(defparameter *input* (uiop:read-file-string "input"))

(defun part1 (input)
  (let ((sum 0))
    (ppcre:do-register-groups ((#'parse-integer a b)) ("mul\\((\\d+),(\\d+)\\)" input)
      (setf sum (+ sum (* a b))))
    sum))

(defun part2 (input)
  (let ((sum 0)
	(state 'enabled))
    (ppcre:do-register-groups (enable (#'parse-integer a b))
	("(don't\\(\\)|do\\(\\))|mul\\((\\d+),(\\d+)\\)" input)
      (when (and (eq state 'enabled) (string= enable "don't()")
		 (setf state 'disabled)))
      (when (and (eq state 'disabled) (string= enable "do()")
		 (setf state 'enabled)))
      (when (and (eq state 'enabled) a b)
	         (setf sum (+ sum (* a b)))))
    sum))

(format t "Part1: ~d~%" (part1 *input*))
(format t "Part2: ~d~%" (part2 *input*))
