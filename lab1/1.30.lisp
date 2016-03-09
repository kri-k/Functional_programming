(defun find-intermediate-point (x1 y1 x2 y2)
    (let (dx dy d-diag new-x2 new-y2 inter-x inter-y)
        (cond ((or (< x2 x1) (and (= x1 x2) (< y2 y1)))
            (psetq x1 x2 x2 x1)
            (psetq y1 y2 y2 y1)))
        (setf dx (- 1 x1))
        (setf dy (- 1 y1))
        (setf new-x2 (+ x2 dx)) 
        (setf new-y2 (+ y2 dy))
        (setf d-diag (- 1 new-x2))
        (incf new-x2 d-diag)
        (incf new-y2 (- d-diag))
        (setf d-diag (/ (- new-y2 1) 2))
        (setf inter-x (+ 1 d-diag (- dx)))
        (setf inter-y (+ 1 d-diag (- dy)))
        (if (and (< inter-x 9) (> inter-x 0) (< inter-y 9) (> inter-y 0))
            (values inter-x inter-y)
            (values (+ x1 (- x2 inter-x)) (- y1 (- inter-y y2))))))

(defun bishop-moves (k l m n)
    (cond ((= (abs (- k m)) (abs (- l n))) t)
          ((not (= (mod (+ k l) 2) (mod (+ m n) 2))) nil)
          (t (find-intermediate-point k l m n))))

(defun pretty-print (f k l m n &optional (str ""))
    (multiple-value-bind (i j) (funcall f k l m n)
        (if j
            (format t "~d ~d" i j)
            (format t "~d" i))
        (format t "~a" str)))

(defun check-all-fields ()
    (loop for x1 from 1 below 9 do
        (loop for y1 from 1 below 9 do
            (loop for x2 from 1 below 9 do
                (loop for y2 from 1 below 9 do
                    (format t "~d ~d, " x1 y1)
                    (format t "~d ~d => " x2 y2)
                    (pretty-print #'bishop-moves x1 y1 x2 y2 #\newline))))))

(defun main ()
    (format t "(bishop-moves ~d ~d ~d ~d) => " 4 4 7 7)
    (pretty-print #'bishop-moves 4 4 7 7 #\newline)
    (format t "(bishop-moves ~d ~d ~d ~d) => " 1 1 2 7)
    (pretty-print #'bishop-moves 1 1 2 7 #\newline))

(check-all-fields)
;(main)
