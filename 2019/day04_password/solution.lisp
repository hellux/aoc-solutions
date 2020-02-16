; leftward part of number starting from given tenth power
(defun init (num power)
  (floor num (expt 10 power)))

; digit at given tenth power of number
(defun dig (num power)
  (mod (init num power) 10))

(defun double-digits-p (num)
  (and (> (init num 1) 0)
       (or (eq (dig num 0) (dig num 1))
           (double-digits-p (init num 1)))))

; number of times least significant digit repeats in a row
; e.g: 2111 -> 3 times, 121 -> 1 time
(defun repeating-count-from-right (num len)
  (if (> len 1)
    (if (= (dig num 0) (dig num 1))
        (+ 1 (repeating-count-from-right (init num 1) (- len 1)))
        1)
    1))

; string contains double digits that are not triple or more digits
(defun only-double-digits-p (num len)
  (let ((repeatc (repeating-count-from-right num len)))
    (and (> len 1)
         (or (= repeatc 2)
             (only-double-digits-p (init num repeatc) (- len repeatc))))))

(defun no-decrease-p (num)
  (or (eq (init num 1) 0)
      (and (<= (dig num 1) (dig num 0))
           (no-decrease-p (init num 1)))))

(defun part1-compliant-p (num)
  (and (double-digits-p num)
       (no-decrease-p num)))

(defun part1 (nums)
  (length (remove-if-not #'part1-compliant-p nums)))

(defun part2-compliant-p (num)
  (and (only-double-digits-p num 6)
       (no-decrease-p num)))

(defun part2 (nums)
  (length (remove-if-not #'part2-compliant-p nums)))

(defun get-range ()
  (let* ((input (read-line *standard-input* nil))
         (dash-pos (search "-" input))
         (start (subseq input 0 dash-pos))
         (end (subseq input (+ 1 dash-pos))))
    (list (parse-integer start) (parse-integer end))))

; generate list of integers from start to end inclusive.
(defun range (start end)
   (loop for i from start below (+ 1 end) by 1 collect i))

(defun main ()
  (let ((nums (eval `(range ,@(get-range)))))
    (format t "part1: ~d~%" (part1 nums))
    (format t "part2: ~d~%" (part2 nums))))
