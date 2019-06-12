;;; Create a hash map of all places visited with given directions starting from
;;; given location.
(defun visits (directions &optional
                          (location #C(0 0))
                          (visited (make-hash-table)))
  (setf (gethash location visited) t)
  (if directions
    (visits (cdr directions) (+ location (car directions)) visited)
    visited))

;;; Merge two hash tables (all values set to t)
(defun hash-table-union (ht1 ht2)
  (loop for loc being the hash-keys of ht2 do (setf (gethash loc ht1) t))
  ht1)

(defun part1 (dirs)
  (hash-table-count (visits dirs)))

(defun part2 (dirs)
  (let ((santa-dirs (loop for d in dirs for i from 0 if (evenp i) collect d))
        (robot-dirs (loop for d in dirs for i from 0 if (oddp i) collect d)))
    (hash-table-count (hash-table-union (visits santa-dirs)
                                        (visits robot-dirs)))))

;;; Turn char directions into complex directions: "<>^" -> (-1 1 i)
(defun parse (input)
  (flet ((char->dir (c) (case c (#\> #C(1 0))
                                (#\^ #C(0 1))
                                (#\< #C(-1 0))
                                (#\v #C(0 -1))
                                (otherwise #C(0 0)))))
    (map 'list #'char->dir input)))

(defun main ()
  (let ((directions (parse (read-line))))
    (format t "part1: ~d~%" (part1 directions))
    (format t "part2: ~d~%" (part2 directions))))
