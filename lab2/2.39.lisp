(defun набор (x) (_set () x))

(defun _set (a b)
	(cond ((car-listp b) (_set (_set a (car b)) (cdr b)))
		((null b) a)
		(t (_set (append a (list (car b))) (cdr b)))))

(defun car-listp (a)
	(and (listp (car a)) (not (null a))))

(print (набор '(A B C D E)))
(print (набор (list 'A 'B (list) 'D 'E)))
(print (набор (list)))
(print (набор (list 'A (list 'B 'C) 'D)))
(print (набор (list (list (list 'A 'B) 'C) (list 'D 'E))))
(print (набор (list (list (list (list 'A))))))
(print (набор (list 'A (list 'B (list 'C (list 'D) 'E) 'F) 'G)))
(print (набор (list 'A NIL 'B NIL 'C)))
