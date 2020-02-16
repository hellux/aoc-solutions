(defconstant +vowels+ "aeiou")
(defconstant +forbidden+ (list "ab" "cd" "pq" "xy"))

(defun three-vowels-p (str)
  (>= (reduce '+ (map 'list (lambda (c) (count c str)) +vowels+)) 3))

(defun duplicate-letters-p (str &optional prev)
  (and (> (length str) 0)
       (or (eq (elt str 0) prev)
           (duplicate-letters-p (subseq str 1) (elt str 0)))))

(defun no-forbidden-strings-p (str)
  (not (eval `(or ,@(map 'list (lambda (s) (search s str)) +forbidden+)))))

(defun nicep (str)
  (and (three-vowels-p str)
       (duplicate-letters-p str)
       (no-forbidden-strings-p str)))

(defun part1 (dirs)
  (length (remove-if-not #'nicep dirs)))

(defun get-lines ()
  (loop for l = (read-line *standard-input* nil) until (null l) collecting l))

(defun main ()
  (let ((strings (get-lines)))
    (format t "part1: ~d~%" (part1 strings))))
