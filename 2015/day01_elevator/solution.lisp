;;; Determine final floor by summing floor changes.
(defun part1 (floor-changes)
  (reduce '+ floor-changes))

;;; Find floor-changes where floor -1 is reached first time.
(defun part2 (floor-changes &optional (current-floor 0) (pos 0))
  (if (< current-floor 0)
    pos
    (if floor-changes
      (part2 (cdr floor-changes)
             (+ current-floor (car floor-changes))
             (+ pos 1))
      0)))

;;; Turn instructions into to floor changes: "()()" -> (1 -1 1 -1).
(defun parse (input)
  (map 'list #'(lambda (c) (if (char= c #\() 1 -1)) input))

(defun main ()
  (let ((floor-changes (parse (read-line))))
    (format t "part1: ~d~%" (part1 floor-changes))
    (format t "part2: ~d~%" (part2 floor-changes))))
