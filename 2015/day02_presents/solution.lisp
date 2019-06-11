;;; Calculate the areas of all sides of an n-dimensional rectangular prism.
;;; Example: (2 3 4) -> (6 8 12)
(defun calc-sides (dims)
  (if (> (list-length dims) 1)
    (concatenate 'list
                 (map 'list
                      (lambda (side) (* (car dims) side))
                      (cdr dims))
                 (calc-sides (cdr dims)))))

;;; Calculate wrapping needed for one present.
(defun wrapping (dims)
  (let ((sides (calc-sides dims)))
    (+ (* 2 (reduce '+ sides)) (reduce 'min sides))))

(defun part1 (dimensions)
  (reduce '+ (map 'list #'wrapping dimensions)))

(defun get-lines ()
  (loop for l = (read-line *standard-input* nil) until (null l) collecting l))

(defun split-on (delim str)
  (let ((next (position delim str)))
    (cond ((equal next nil) (list str))
          ((equal next 0) (split-on delim (subseq str 1)))
          (t
            (let ((head (subseq str 0 next))
                  (tail (subseq str next)))
              (cons head (split-on delim tail)))))))
        
(defun parse-line (line)
  (map 'list #'parse-integer (split-on #\x line)))

(defun main ()
  (let ((dimensions (map 'list #'parse-line (get-lines))))
    (format t "part1: ~d~%" (part1 dimensions))))
