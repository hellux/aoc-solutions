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

(defun get-lines ()
  (loop for l = (read-line *standard-input* nil) until (null l) collecting l))

(defun main ()
  (let ((numbers (map 'list #'parse-integer (get-lines))))
    (format t "~d~%" (part1 numbers))))
