(defun square (x) (* x x))

(defclass cart () 
    ((x :initarg :x :reader cart-x)
     (y :initarg :y :reader cart-y)))

(defclass polar ()
    ((radius :initarg :radius :accessor radius)
     (angle  :initarg :angle  :accessor angle)))

(defmethod cart-x ((p polar))
    (* (radius p) (cos (angle p))))

(defmethod cart-y ((p polar))
    (* (radius p) (sin (angle p))))

(defmethod print-object ((c cart) stream)
    (format stream "[CART x ~d y ~d]"
        (cart-x c) (cart-y c)))

(defgeneric to-cart (arg)
    (:documentation "Преобразование аргумента в декартову систему.")
    (:method ((c cart)) c)
    (:method ((p polar))
        (make-instance 'cart
            :x (cart-x p)
            :y (cart-y p))))

(defmethod radius ((c cart))
    (sqrt (+ (square (cart-x c))
        (square (cart-y c)))))

(defmethod angle ((c cart))
    (atan (cart-y c) (cart-x c)))

(defmethod print-object ((p polar) stream)
    (format stream "[POLAR radius ~d angle ~d]"
        (radius p) (angle p)))

(defgeneric to-polar (arg)
    (:documentation "Преобразование аргумента в полярную систему.")
    (:method ((p polar)) p)
    (:method ((c cart))
        (make-instance 'polar
            :radius (radius c)
            :angle (angle c))))

(defclass line ()
    ((start :initarg :start :accessor line-start)
     (end   :initarg :end   :accessor line-end)))

(defun A (line)
    (let ((p1 (line-start line)) (p2 (line-end line)))
        (- (cart-y p1) (cart-y p2))))

(defun B (line)
    (let ((p1 (line-start line)) (p2 (line-end line)))
        (- (cart-x p2) (cart-x p1))))

(defun C (line)
    (let ((p1 (line-start line)) (p2 (line-end line)))
        (- (* (cart-x p1) (cart-y p2)) (* (cart-y p1) (cart-x p2)))))

(defun between (a b c) 
    (if (> a c) (psetq a c c a))
    (and (<= a b) (<= b c)))

(defun point-on-linep (line point)
    (let ((ps (line-start line)) (pe (line-end line)))
        (and (between (cart-x ps) (cart-x point) (cart-x pe))
            (between (cart-y ps) (cart-y point) (cart-y pe)))))

(defun two-lines-intersection (l1 l2)
    (let ((a1 (A l1)) (b1 (B l1)) (c1 (C l1)) 
          (a2 (A l2)) (b2 (B l2)) (c2 (C l2))
          x y)
        (setq x (/ (- (* b2 c1) (* b1 c2)) (- (* a2 b1) (* a1 b2))))
        (setq y (/ (- (* a2 c1) (* a1 c2)) (- (* a1 b2) (* a2 b1))))
        (make-instance 'cart :x x :y y)))

(defun line-intersections (lines)
    (let ((inters '()) (tmp-point))
        (loop with n = (length lines)
            for i upfrom 0 below n do
            (loop for j upfrom (1+ i) below n do
                (setq tmp-point (two-lines-intersection (nth i lines) (nth j lines)))
                (if (and (point-on-linep (nth i lines) tmp-point) (point-on-linep (nth j lines) tmp-point))
                    (setq inters (cons (list i j tmp-point) inters)))))
        inters))

(defun test-1 ()
    (let (l1 l2 l3 l4 inter-list)
        (setq l1 (make-instance 'line
            :start (make-instance 'cart :x 0 :y 0)
            :end (make-instance 'cart :x 3 :y 3)))

        (setq l2 (make-instance 'line
            :start (to-polar (make-instance 'cart :x 0 :y 4))
            :end (to-polar (make-instance 'cart :x 4 :y 0))))

        (setq l3 (make-instance 'line
            :start (make-instance 'cart :x 0 :y 2)
            :end (make-instance 'cart :x 4 :y 2)))

        (setq l4 (make-instance 'line
            :start (make-instance 'cart :x 2 :y 0)
            :end (make-instance 'cart :x 2 :y 5)))

        (setq inter-list (line-intersections (list l1 l2 l3 l4)))
        (format t "~{~A~^~%~}~%" inter-list)))

(defun test-2 ()
    (let (l1 l2 l3)
        (setq l1 (make-instance 'line
            :start (make-instance 'cart :x 0 :y 0)
            :end (make-instance 'cart :x 0 :y 3)))

        (setq l2 (make-instance 'line
            :start (make-instance 'cart :x 0 :y 9)
            :end (make-instance 'cart :x 3 :y 0)))

        (setq l3 (make-instance 'line
            :start (make-instance 'cart :x 0 :y 0)
            :end (make-instance 'cart :x 0 :y 10)))
        (format t "l1 & l2:")
        (format t "~{~A~^~%~}~%" (line-intersections (list l1 l2)))
        (format t "l2 & l3:")
        (format t "~{~A~^~%~}~%" (line-intersections (list l2 l3)))))

(format t "test-1~%")
(test-1)
(format t "test-2~%")
(test-2)
