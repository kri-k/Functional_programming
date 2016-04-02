(defun набор (x) (_set () x))

(defun _set (a b)
	(cond ((first-listp b) (_set (_set a (first b)) (rest b)))
		((null b) a)
		(t (_set (append a (list (first b))) (rest b)))))

(defun first-listp (a)
	(and (listp (first a)) (not (null a))))

(print (набор '(A B C D E)))
(print (набор (list 'A 'B (list) 'D 'E)))
(print (набор (list)))
(print (набор (list 'A (list 'B 'C) 'D)))
(print (набор (list (list (list 'A 'B) 'C) (list 'D 'E))))
(print (набор (list (list (list (list 'A))))))
(print (набор (list 'A (list 'B (list 'C (list 'D) 'E) 'F) 'G)))
(print (набор (list 'A NIL 'B NIL 'C)))
