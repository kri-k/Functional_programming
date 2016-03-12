(defun one-move (x1 y1 x2 y2)
    (= (abs (- x1 x2)) (abs (- y1 y2))))

(defun get-x (a)
    (+ (div (- a 1) 8) 1))

(defun get-y (a)
    (+ (mod (- a 1) 8) 1))

(defun div (a b)
    (/ (- a (mod a b)) b))

(defun find-intermediate-point (x1 y1 x2 y2 inter)
    (cond ((one-move x1 y1 x2 y2) T)
          ((> inter 64) nil)
          ((and (one-move x1 y1 (get-x inter) (get-y inter))
                (one-move x2 y2 (get-x inter) (get-y inter)))
            (values (get-x inter) (get-y inter)))
          (T (find-intermediate-point x1 y1 x2 y2 (+ inter 1)))))

(defun bishop-moves (k l m n)
    (find-intermediate-point k l m n 1))

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
