(defun copy-txt (text)
    (mapcar (lambda (str) (subseq str 0)) text))

(defun print-txt (text)
    (format t "~{~A~^ ~}~%" text))

(defun proc-txt (text)
    (if (text-with-plusp text)
        (let ((new-text (copy-txt text)))
            (__proc-txt new-text)
            new-text)
        text))

(defun __proc-txt (text)
    (let ((pos (position #\+ (first text))))
        (if (not (null pos))
            (proc-str (first text) pos)
            (if (__proc-txt (rest text))
                (proc-str (first text))
                NIL))))

(defun proc-str (str &optional (pos (length str)))
    (loop for i upfrom 0 below pos do
        (if (digit-char-p (aref str i))
            (setf (aref str i) #\-)))
    T)

(defun text-with-plusp (text)
    (member #\+ (mapcar (lambda (str) (find #\+ str)) text)))


(defun test (text)
    (format t "original text:~%")
    (print-txt text)
    (format t "processed text:~%")
    (print-txt (proc-txt text))
    (format t "original text is still:~%")
    (print-txt text)
    (format t "===============~%"))

(defvar txt1 '("a1a3a"
               "b4b+b5"
               "c9+7c"))

(defvar txt2 '("a1a3a"
               "b4b0b5"
               "c907c"))

(defvar txt3 '("aaaaa"
               "bbb+bb"
               "cc+cc"))

(defvar txt4 '("+aaaa"
               "bbbbb"
               "ccccc"))

(test txt1)
(test txt2)
(test txt3)
(test txt4)
