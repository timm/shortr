;vim: ts=2 sw=2 et :
(make-package :chops)
(in-package   :chops)

(defstruct (settings (:conc-name ?)) 
  (inf  most-positive-fixnum)
  (ninf most-negative-fixnum)
  (file 'nasa93dem)
  (p    2)) 

(defvar my (make-settings))

(defun stop () #+sbcl (sb-ext:exit) #+:clisp (ext:exit))

(defmacro aif (test then else)
  `(let (it) (if (setf it ,test) ,then ,else)))

(defmacro ?  (p x &rest xs)
  (if (null xs) `(slot-value ,p ',x) `(? (slot-value ,p ',x) ,@xs)))

(defmacro has (x a)
   `(cdr (or (assoc ,x ,a :test #'equal)
             (car (setf ,a (cons (cons ,x 0) ,a))))))

(defmethod str2thing ((x string))
  (let ((x (string-trim '(#\Space #\Tab) x)))
    (if (equal x "?") 
      #\?
      (let ((y (ignore-errors (read-from-string x))))
        (if (numberp y) y x))))))

(defun str2things (s &optional (sep #\,) (x 0) (y (position sep s :start (1+ x))))
  (cons (str2thing (subseq s x y)) (and y (str2list s sep (1+ y)))))

(defmacro with-csv ((lst file &optional out) &body body)
  (let ((str (gensym)))
      `(let (,lst) 
         (with-open-file (,str ,file)
           (loop while (setf ,lst (read-line ,str nil)) do 
             (setf ,lst (str2things ,lst))
             ,@body))
         ,out)))

(defmacro with-csv ((row file &optional out) &body body)
  (let ((x (gensym)))
    `(lines ,file (lambda (,x) 
                    (let ((,x (str2list ,x))) ,@body)))))

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
  (at 0) (txt "") (w 1) (n 0) (mu 0) (hi (?ninf my)) (lo (?inf my)))

(defun make-num (&key (at 0) (txt ""))
  (%make-num :at at :txt txt :w (if (lessp txt) -1 1)))

(defmethod norm ((self num) x)
  (with-slots (lo hi) self
    (if (< (- hi lo) 1E9) (/ (- x lo) (- hi lo)))))

(defmethod add ((self num) x)
  (with-slots (n lo hi mu) self 
    (when (not (eq x #\?))
      (incf n)
      (incf mu (/ (- x mu) n))
      (setf lo (min x lo)
            hi (max x hi)))))

(defmethod mid ((self num)) (? self mu))

(defstruct sym (at 0) (txt "") (n 0) mode (most 0) has)
(defmethod add ((self sym) x)
  (with-slots (n has most mode) self 
    (when (not (eq x #\?))
      (incf n)
      (incf (has x has))
      (if (> n most) (setf most n
                           mode x)))))

(defmethod mid ((self sym)) (? self mode))

(defstruct row has within)

(defmethod lt ((i row) (j row))
  (let* ((s1 0) (s2 0) 
         (ys (? i within cols ys))  (n (length ys)))
    (dolist (y ys (< (/ s1 n) (/ s2 n)))
      (let ((a (norm y (elt (? i has) (? y at))))
            (b (norm y (elt (? j has) (? y at)))))
        (decf s1 (exp (/  (* (? y w) (- a b)) n)))
        (decf s2 (exp (/  (* (? y w) (- b a)) n)))))))

(defstruct (cols (:constructor %make-cols)) names has xs ys)
(defun make-cols (lst)
  (let (has xs ys (at -1)) 
    (dolist (name lst)
      (let* ((what (if (nump name) 'make-num 'make-sym))
             (col  (funcall what :at (incf at):txt name)))
        (push col has)
        (when (not (skipp name))
          (if (goalp name) (push col ys) (push col xs)))))
    (%make-cols :has (reverse has) :xs xs :ys ys :names lst)))

(defstruct (egs (:constructor %make-egs)) has cols)
(defun make-egs (&optional src &aux (self (%make-egs)))
  (typecase src
     (null   self)
     (cons   (dolist (row src self) (add self row)))
     (symbol (make-egs (get-file src)))))

(defmethod add ((self egs) (r cons)) (add self (make-row :has r :within self)))
(defmethod add ((self egs) (r row))
  (with-slots (has cols) self
    (cond ((null cols) (setf cols (make-cols (row-has r))))
          (t           (mapcar 'add (cols-has cols) (row-has r)) 
                       (push r has)))))

(let ((all (get-file (?file my)))) 
    (make-cols (car all)))

(let ((e (make-egs (?file my))))
  (setf (? e has ) (sort (? e has) #'lt))
  (dotimes (i 10) (print (? (elt (? e has) i) has))))
