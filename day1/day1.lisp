(defparameter *input-path* "input")

(defun get-input ()
  (uiop:read-file-lines *input-path*))

(defun split-line (line)
  (cl-utilities:split-sequence #\Space line :remove-empty-subseqs t))

(defun separate (input a b)
  (cond ((eq input nil) (list a b))
    (t (separate (cdr input) (push (parse-integer (caar input)) a) (push (parse-integer (cadar input)) b)))))

(defun split (input output)
  (cond ((eq input nil) output)
    (t (split (cdr input) (append output (list (split-line (car input))))))))

(defun parse-input () 
  (separate (split (get-input) ()) () ()))
 
(defun sort-a-b (list-arg)
  (list (sort (first list-arg) #'<) (sort (second list-arg) #'<)))

(defun diffs (arg1 arg2 res)
  (cond ((eq arg1 nil) res)
    (t (diffs (cdr arg1) (cdr arg2) (+ res (abs (- (car arg1) (car arg2))))))))

(let* ((sorted-input (sort-a-b (parse-input)))
       (a (first sorted-input))
       (b (second sorted-input)))
  (print (diffs a b 0)))
