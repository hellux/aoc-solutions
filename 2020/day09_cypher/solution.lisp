(defparameter *preamble-size* 25)

(defun sum-p (des asc num)
  (if (or (null asc) (null des))
    nil
    (let ((sum (+ (car des) (car asc))))
      (cond ((= sum num) t)
            ((> sum num) (sum-p (cdr des) asc num))
            ((< sum num) (sum-p des (cdr asc) num))))))

(defun invalid-p (preamble num)
  (let* ((des (sort (copy-seq preamble) #'>))
         (asc (reverse des)))
  (not (sum-p des asc num))))

(defun find-invalid (preamble remaining)
  (cond ((null remaining) nil)
        ((invalid-p preamble (car remaining)) (car remaining))
        (t (find-invalid (concatenate 'list
                                      (cdr preamble)
                                      (list (car remaining)))
                         (cdr remaining)))))

(defun part1 (numbers)
  (find-invalid (subseq numbers 0 *preamble-size*)
                (subseq numbers *preamble-size*)))

(defun find-sum (current remaining target)
  (let ((sum (reduce '+ current)))
    (cond ((= sum target) current)
          ((> sum target) (find-sum (subseq current 0 (- (length current) 1))
                                    remaining
                                    target))
          ((< sum target) (find-sum (cons (car remaining) current)
                                    (cdr remaining)
                                    target)))))

(defun part2 (numbers inv)
  (let ((seq (find-sum '() numbers inv)))
    (+ (reduce #'min seq) (reduce #'max seq))))

(defun get-lines ()
  (loop for l = (read-line *standard-input* nil) until (null l) collecting l))

(defun main ()
  (let* ((numbers (map 'list #'parse-integer (get-lines)))
         (p1 (part1 numbers))
         (p2 (part2 numbers p1)))
    (format t "~d~%~d~%" p1 p2)))
