(defconstant +password-length+ 8)
(defconstant +zero-code+ (char-code #\a))
(defconstant +base+ 26)

(defun pw-to-pows (pw)
  (map 'list #'(lambda (c) (- (char-code c) +zero-code+)) pw))

(defconstant +forbidden-pows+ (pw-to-pows "ilo"))

(defun pows-to-pw (pows)
  (coerce (map 'list #'(lambda (p) (code-char (+ +zero-code+ p))) pows)
          'string))

(defun increment-forbidden (pows)
  (map 'list #'(lambda (p) (if (member p +forbidden-pows+)
                             (+ p 1)
                             (identity p)))
       pows))

(defun pows-to-num (pows)
  (if (> (length pows) 0)
    (+ (* (car pows) (expt +base+ (length (cdr pows))))
       (pows-to-num (cdr pows)))
    (identity 0)))

(defun num-to-pows (num &optional (pow (- +password-length+ 1)))
  (if (>= pow 0)
    (let ((magnitude (expt +base+ pow)))
      (cons (floor num magnitude)
            (num-to-pows (mod num magnitude) (- pow 1))))
    (identity ())))

(defun increment (pows)
  (num-to-pows (+ 1 (pows-to-num pows))))

(defun pair (pows &optional ign)
  (and (>= (length pows) 2)
       (if (and (eq (car pows) (elt pows 1)) (not (eq ign (car pows))))
         (car pows)
         (pair (cdr pows) ign))))

(defun two-pairs-p (pows)
  (pair pows (pair pows)))

(defun successive-triple-p (pows)
  (and (>= (length pows) 3)
       (let ((e0 (elt pows 0)) (e1 (elt pows 1)) (e2 (elt pows 2)))
         (or (and (= e1 (+ e0 1)) (= e2 (+ e1 1)))
             (successive-triple-p (cdr pows))))))

(defun next-password (pows)
  (let ((filtered (increment-forbidden pows)))
    (if (and (successive-triple-p filtered) (two-pairs-p filtered))
      (identity filtered)
      (next-password (increment filtered)))))

(defun part1 (pows)
  (next-password pows))

(defun part2 (pows)
  (next-password (increment (part1 pows))))

(defun main ()
  (let* ((password (read-line *standard-input* nil))
         (pows (pw-to-pows password)))
    (format t "~d~%" (pows-to-pw (part1 pows)))
    (format t "~d~%" (pows-to-pw (part2 pows)))))
