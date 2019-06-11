;;; Determine final floor from list of floor changes.
(defun part1 (floor-changes)
  (reduce '+ floor-changes))

;;; Find floor change where floor -1 is reached for the first time.
(defun part2 (floor-changes &optional (current-floor 0) (pos 0))
  (if (< current-floor 0)
    pos
    (if floor-changes
      (part2 (cdr floor-changes)
             (+ current-floor (car floor-changes))
             (+ pos 1))
      0)))

;;; Turn instructions into floor changes: "()()" -> (1 -1 1 -1).
(defun parse (input)
  (defun char->change (c)
    (cond ((char= c #\() 1)
          ((char= c #\)) -1)
          (t 0)))
  (map 'list #'char->change input))

(defun main ()
  (let ((floor-changes (parse (read-line))))
    (format t "part1: ~d~%" (part1 floor-changes))
    (format t "part2: ~d~%" (part2 floor-changes))))
