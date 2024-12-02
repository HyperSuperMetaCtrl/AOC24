(ql:quickload "str")
(defparameter *input-path* "input")

(defun parse-input (path)
  (loop for line in (uiop:read-file-lines path)
	collect (mapcar #'parse-integer (str:split " " line))))

(defun is-increasing (lst)
  (every #'< lst (rest lst)))

(defun is-decreasing (lst)
  (every #'> lst (rest lst)))

(defun is-safe-diff-p (a b)
  (<= (abs (- a b)) 3))

(defun is-safe (lst)
  (and (or (is-increasing lst) (is-decreasing lst))
       (every #'is-safe-diff-p lst (rest lst))))

(defun remove-nth (n lst)
  (remove-if (constantly t) lst :start n :count 1))

(defun can-be-fixed (lst)
  (loop for i from 0 below (length lst)
	thereis (is-safe (remove-nth i lst))))

(defun is-safe-part-2 (lst)
  (or (is-safe lst) (can-be-fixed lst)))

(let* ((input (parse-input *input-path*))
       (safe-reports-part-1 (count-if #'is-safe input))
       (safe-reports-part-2 (count-if #'is-safe-part-2 input)))

  (format t "Number of safe reports Part 1: ~d~%" safe-reports-part-1)
  (format t "Number of safe reports Part 2: ~d~%" safe-reports-part-2))
