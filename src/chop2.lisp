;vim: ts=2 sw=2 et :
(make-package :chops)
(in-package   :chops)

(defstruct (settings (:conc-name ?)) 
  (inf  most-positive-fixnum)
  (ninf most-negative-fixnum)
  (file 'nasa93dem)
  (p    2)) 

(defvar my (make-settings))

(defmacro aif (test then else)
  `(let (it) (if (setf it ,test) ,then ,else)))

(defmacro ?  (p x &rest xs)
  (if (null xs) `(slot-value ,p ',x) `(? (slot-value ,p ',x) ,@xs)))

(defmacro has (x a)
   `(cdr (or (assoc ,x ,a :test #'equal)
             (car (setf ,a (cons (cons ,x 0) ,a))))))

(labels ((chars (s) (if (stringp s) s (symbol-name s)))
         (chrn  (x) (let ((s (chars x))) (char s (1- (length s)))))
         (chr0  (x) (char (chars x) 0)))
  (defun skipp(x)  (eq          (chrn x) #\X))
  (defun lessp(x)  (eq          (chrn x) #\-))
  (defun goalp(x)  (member      (chrn x) '(#\- #\+ #\!)))
  (defun nump (x)  (eq (chr0 x) #\$)))

(defun get-file (file) 
  (with-open-file (str (format nil "~a.lisp" file)) (read str nil)))

(defun order (n)
  (lambda (x y)
    (let ((x (elt x n))
          (y (elt y n)))
      (let ((x (if (eq x #\?) (?ninf my) x))
            (y (if (eq y #\?) (?ninf my) y)))
        (if (numberp x) (< x y) (string< x y))))))

(defun cell (row at &aux (it (elt row at)))
  (if (not (eq it #\?)) it))

(defstruct (num (:constructor %make-num))
  (at 0) (txt "") (w 1) (n 0) (hi (?ninf my)) (lo (?inf my)))

(defun make-num (&key (at 0) (txt ""))
  (%make-num :at at :txt txt :w (if (lessp txt) -1 1)))

(defmethod add ((self num) x)
  (with-slots (n lo hi ) self 
    (when (not (eq x #\?))
      (incf n)
      (setf lo (min x lo)
            hi (max x hi)))))

(defstruct sym (at 0) (txt "") (n 0) has)
(defmethod add ((self sym) x)
  (with-slots (n has) self 
    (when (not (eq x #\?))
      (incf n)
      (incf (has x has)))))

(defstruct row has within)

(defstruct (cols (:constructor %make-cols)) names has x y)
(defun make-cols (lst)
  (let (has x y (at 0)) 
    (dolist (name lst)
      (let* ((what (if (nump name) 'make-num 'make-sym))
             (col  (funcall what :at (incf at):txt name)))
        (push col has)
        (when (not (skipp name))
          (if (goalp name) (push col y) (push col x)))))
    (%make-cols :has (reverse has) :x x :y y :names lst)))

(defstruct (egs (:constructor %make-egs)) has cols)
(defun make-egs (&optional src &aux (self (%make-egs)))
  (cond ((null src)  self)
        ((consp src) (dolist (row  src self) (add self row)))
        (t           (make-egs (get-file src)))))

(defmethod add ((self egs) row)
  (with-slots (has cols) self
    (cond ((consp row) (add self (make-row :has row :within self)))
          ((null cols) (setf cols (make-cols row)))
          (t           (mapcar 'add cols (row-has row))
                       (push row has)))))

(let ((all (get-file (?file my)))) 
    (make-cols (car all)))

(make-egs (?file my))

