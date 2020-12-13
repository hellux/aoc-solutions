(defun time-to-wait (from x) (- x (rem from x)))
(defun part1 (from schedule)
  (let ((best (car (sort schedule
                           (lambda (x y) (< (time-to-wait from x)
                                            (time-to-wait from y)))))))
    (* best (time-to-wait from best))))

(defun split-on (delim str)
  (let ((next (position delim str)))
    (cond ((equal next nil) (list str))
          ((equal next 0) (split-on delim (subseq str 1)))
          (t
            (let ((head (subseq str 0 next))
                  (tail (subseq str next)))
              (cons head (split-on delim tail)))))))

(defun xp (c) (string-equal c "x"))

(defun main ()
  (let* ((earliest (parse-integer (read-line)))
         (schedule (split-on #\, (read-line)))
         (nums (map 'list #'parse-integer (remove-if #'xp schedule))))
    (format t "part1: ~d~%" (part1 earliest nums))))
