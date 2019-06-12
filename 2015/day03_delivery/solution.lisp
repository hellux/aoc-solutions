(defun parse (input)
  (flet ((char->dir (c) (case c (#\> #C(1 0))
                                (#\^ #C(0 1))
                                (#\< #C(-1 0))
                                (#\v #C(0 -1))
                                (otherwise #C(0 0)))))
    (map 'list #'char->dir input)))

;;; Creat a hash map of all places visited with given directions starting from
;;; given location.
(defun visits (directions &optional
               (location #C(0 0))
               (visited (make-hash-table)))
  (setf (gethash location visited) t)
  (if directions
    (visits (cdr directions) (+ location (car directions)) visited)
    visited))

(defun part1 (directions)
  (hash-table-count (visits directions)))

(defun main ()
  (let ((directions (parse (read-line))))
    (format t "part1: ~d~%" (part1 directions))))
