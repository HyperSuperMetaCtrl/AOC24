(ql:quickload "cl-ppcre")

(defparameter *input* (uiop:read-file-string "input"))

(defun part1 (input)
  (let ((sum 0))
    (ppcre:do-register-groups ((#'parse-integer a b)) ("mul\\((\\d+),(\\d+)\\)" input)
      (setq sum (+ sum (* a b))))
    sum))

(format t "Part1: ~d~%" (part1 *input*))


