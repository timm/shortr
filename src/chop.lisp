;  vim: ts=2 sw=2 et : 
(defun get-file (file) 
  (with-open-file (str file) (read str nil)))

(defun chrn (x &optional (s (string x))) (char s (1- (length s))))
(defun chr0 (x) (member (char (string x) 0) lst))

(defun goalp(x) (member (chrn (string x)) '(#\- #\+ #\!)))
(defun nump(x) (upper-case-p (chr0 x)))

;(print (get-file "../etc/data/data.lisp"))

(defvar *big* most-positive-fixnum)

(defmacro defthing (x &rest lst) 
  (let ((f (intern (string-upcase (format nil "%MAKE-~a" x))))) 
    `(defstruct (,x (:constructor ,f)) ,@lst)))

(defthing num (n 0) (at 0) (txt "") (lo *big*) (hi (* -1 *big*)))
(defthing sym (n 0) (at 0) (txt "") (mu 0) (m2 0) (sd 0) (w 1))


