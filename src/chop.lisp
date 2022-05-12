(defun get-file (file) 
  (with-open-file (str file) (read str nil)))

(defun charn (x &optional (str (string x))) (char str (1- (length str)))) 
(defun char0 (x &optional (str (string x))) (char str 0))

(defun goalp(x) (member (charn (string x)) '(#\- #\+ #\!)))

;(print (get-file "../etc/data/data.lisp"))

(defvar *big* most-positive-fuxnum)

(defstruct num (at 0) (txt "") (lo *big*) (hi (1- ssbig*)))

