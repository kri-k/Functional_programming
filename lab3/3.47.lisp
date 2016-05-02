(defun print-matrix (matrix &optional (chars 3) stream)
    (let ((*print-right-margin* (+ 6 (* (1+ chars) 
                                        (array-dimension matrix 1)))))
        (pprint matrix stream)
        (values)))

(defun swap-rows (mtrx r1 r2)
    (loop with n = (array-dimension mtrx 1)
        for i upfrom 0 below n do
            (rotatef (aref mtrx r1 i) (aref mtrx r2 i)))
    mtrx)

(defun swap-cols (mtrx c1 c2)
    (loop with n = (array-dimension mtrx 0)
        for i upfrom 0 below n do
            (rotatef (aref mtrx i c1) (aref mtrx i c2)))
    mtrx)

(defun pos-of-max (mtrx)
    (let ((max-row 0) (max-column 0) (max-elem (abs (aref mtrx 0 0))))
        (loop with n = (array-dimension mtrx 0)
            for i upfrom 0 below n do
            (loop with m = (array-dimension mtrx 1) 
            for j upfrom 0 below m do
                (cond ((> (abs (aref mtrx i j)) max-elem) 
                    (setf max-elem (abs (aref mtrx i j)) max-row i max-column j)))))
        (values max-row max-column)))

(defun copy-array (array) 
    (adjust-array array (array-dimensions array)))

(defun swap-max-to-top-left (mtrx)
    (let ((new-mtrx (copy-array mtrx)))
        (multiple-value-bind (i j) (pos-of-max mtrx) 
            (swap-rows new-mtrx 0 i)
            (swap-cols new-mtrx 0 j))
        new-mtrx))


(defvar *m* (make-array '(4 4) :initial-contents '((1 2 3 4) 
                                                    (5 6 7 8) 
                                                    (9 0 -99 2)
                                                    (3 4 5 6))))
(defvar *n* NIL)

(format t "~%original matrix = ")
(print-matrix *m*)
(format t "~%new matrix = ")
(setf *n* (swap-max-to-top-left *m*))
(print-matrix *n*)
(format t "~%original matrix is still = ")
(print-matrix *m*)
