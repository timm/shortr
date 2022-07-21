; vim: et ts=2 tw=2 :
; TIYIN (c) 2022, Tim Menzies 
; Multi-objective semi-supervised XAI.

(defpackage :tiyin (:use :cl))
(in-package :tiyin)
(defvar *opt* 
  `((file  "-f"  "help file"  "../../data/auto93.lisp")
    (help  "-h"  "show help"                 nil)
    (keep  "-K"  "items to keep"             256)
    (k     "-k"  "nb low attributes classes" 1)
    (m     "-n"  "nb low frequency classes"  2)
    (seed  "-s"  "random number seed"        10019)
    (go    "-g"  "start up action"           ls)))

(defmacro ? (s x &rest xs)
  (if (null xs) `(slot-value ,s ',x) `(? (slot-value ,s ',x) ,@xs)))

(defmethod it (x) x)
(defmethod it ((x string))
  (let ((y (string-trim '(#\Space #\Tab #\Newline) x)))
    (if (string= y "?") y
      (let ((z (ignore-errors (read-from-string y))))
        (if (numberp z) z y)))))

(defun cli (flag help default)
  (declare (ignore help))
  (let* ((args #+clisp ext:*args* #+sbcl sb-ext:*posix-argv*)
         (it (member flag args :test 'equal)))
    (cond ((not it) default)
          ((equal default t)   nil)
          ((equal default nil) t)
          (t (it (second it))))))

(setf *opt* (mapcar (lambda (x) (cons (car x) (apply 'cli (cdr x))))*opt*))

(print *opt*)
(defmacro my (x) `(cdr (assoc *opt* ',x)))
(defmacro aif (test then &optional else) `(let ((it ,test)) (if it ,then ,else)))

(with-open-file (stream (my file))
  (loop while (setf if (read stream nil)) do  (print it)))

(defstruct sym (n 0) (txt "") (at 0) has))

(defstruct (num (:constructor %make-num)) (n 0) (txt "") (at 0) has)
(defun make-num (txt at)
  (%make-num :txt txt :at at :w (if (find #\- txt) -1 1)))

(defstruct (cols (:constructor %make-cols)) all x y names)
(defun make-cols (txts &aux (at -1) (self (%make-cols :txts txts)))
  (dolist (txt txts self)
    (let* ((what (if (upper-case-p (char txt 0)) 'make-num 'make-sym))
           (col  (funcall what  txt  (incf at))
      (push (? self all) col)
      (unless (find #\: txt)
        (if (find #\! txt) (setf (? self y klass) col))
        (if (or (find #\- txt) (find #\+ txt) (find #\! txt))
          (push (? self y) col)
          (push (? self x) col))))))

(print *opt*)
