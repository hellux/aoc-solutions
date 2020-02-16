;;; Apply a function to each combination of side lengths.
(defun calc-sides (fn dims)
  (if (> (list-length dims) 1)
    (concatenate 'list
                 (map 'list
                      (lambda (side) (funcall fn (car dims) side))
                      (cdr dims))
                 (calc-sides fn (cdr dims)))))

;;; Calculate the areas of all sides of a present.
;;; Example: (2 3 4) -> (6 8 12)
(defun calc-sides-areas (dims)
  (calc-sides '* dims))

;;; Calculate the circumferences of all sides of a present.
;;; prism.  Example: (2 3 4) -> (10 12 14)
(defun calc-sides-circs (dims)
  (calc-sides (lambda (x y) (+ (* 2 x) (* 2 y))) dims))

;;; Calculate wrapping needed for one present.
(defun wrapping (dims)
  (let ((areas (calc-sides-areas dims)))
    (+ (* 2 (reduce '+ areas)) (reduce 'min areas))))

;;; Calculate length of ribbon needed for one present.
(defun ribbon (dims)
  (+ (reduce 'min (calc-sides-circs dims)) (reduce '* dims)))

(defun part1 (dimensions)
  (reduce '+ (map 'list #'wrapping dimensions)))

(defun part2 (dimensions)
  (reduce '+ (map 'list #'ribbon dimensions)))

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
    (format t "part1: ~d~%" (part1 dimensions))
    (format t "part2: ~d~%" (part2 dimensions))))
