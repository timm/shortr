;vim: ts=2 sw=2 et :
(make-package :chops)
(in-package   :chops)

(defstruct (settings (:conc-name !)) 
  (inf    most-positive-fixnum)
  (ninf   most-negative-fixnum)
  (file   "../etc/data/auto93.csv")
  (round  2)
  (p      2)) 

(defvar my (make-settings))
;--------------------------------------------------------------------
(defmacro ?  (p x &rest xs)
  (if (null xs) `(slot-value ,p ',x) `(? (slot-value ,p ',x) ,@xs)))

(defmacro has (x a)
   `(cdr (or (assoc ,x ,a :test #'equal)
             (car (setf ,a (cons (cons ,x 0) ,a))))))

(defun str2thing (x &aux (y (ignore-errors (read-from-string x))))
  (if (numberp y) y x))

(defun str2things (s 
                   &optional (sep #\,) (x 0) (y (position sep s :start (1+ x))))
  (cons  (subseq s x y) (and y (str2things s sep (1+ y)))))

(defmacro with-csv ((lst file &optional out) &body body)
  (let ((str (gensym)))
      `(let (,lst) 
         (with-open-file (,str ,file)
           (loop while (setf ,lst (read-line ,str nil)) do 
             (setf ,lst (str2things ,lst ))
             ,@body))
         ,out)))

(defun get-file (file) 
  (with-open-file (str (format nil "~a.lisp" file)) (read str nil)))

(defun charn (s &rest lst) (member (char s (1- (length s))) lst)); :test #'eql))

(defun rnd (number &optional (digits (!round my )))
  (let* ((div (expt 10 digits))
           (tmp (/ (round (* number div)) div)))
               (if (zerop digits) (floor tmp) (float tmp))))

(defun rnds (lst) (mapcar #'rnd lst))

(defun stop () #+sbcl (sb-ext:exit) #+:clisp (ext:exit))
;--------------------------------------------------------------------
(defstruct row has within)
(defstruct (cols (:constructor %make-cols)) names has xs ys)
(defstruct (egs (:constructor %make-egs)) has cols)
(defstruct sym (at 0) (txt "") (n 0) mode (most 0) has)
(defstruct (num (:constructor %make-num))
  (at 0) (txt "") (w 1) (n 0) (mu 0) (hi (!ninf my)) (lo (!inf my)))

;--------------------------------------------------------------
(defun make-num (&key (at 0) (txt ""))
  (%make-num :at at :txt txt :w (if (charn txt -1 #\-) -1 1)))

(defmethod add ((self num) (x string))
  (if (string-equal x "?")
    x
    (add self (read-from-string x))))

(defmethod add ((self num) x)
  (with-slots (n lo hi mu) self 
    (incf n)
    (incf mu (/ (- x mu) n))
    (setf lo (min x lo)
          hi (max x hi)))
  x)

(defmethod mid ((self num)) (? self mu))

(defmethod norm ((self num) x)
  (with-slots (lo hi) self
    (if (< (- hi lo) 1E9) (/ (- x lo) (- hi lo)))))
;--------------------------------------------------------------------
(defmethod add ((self sym) x)
  (with-slots (n has most mode) self 
    (unless (string-equal x "?")
      (incf n)
      (incf (has x has))
      (if (> n most) (setf most n
                           mode x))))
  x)

(defmethod mid ((self sym)) (? self mode))
;--------------------------------------------------------------------
(defmethod lt ((i row) (j row))
  (let* ((s1 0) (s2 0) 
         (ys (? i within cols ys))  (n (length ys)))
    (dolist (y ys (< (/ s1 n) (/ s2 n)))
      (let ((a (norm y (elt (? i has) (? y at))))
            (b (norm y (elt (? j has) (? y at)))))
        (decf s1 (exp (/  (* (? y w) (- a b)) n)))
        (decf s2 (exp (/  (* (? y w) (- b a)) n)))))))
;--------------------------------------------------------------------
(defun make-cols (lst)
  (labels ((nump  (s)           (upper-case-p (char s  0)))
           (goalp (s)           (charn s #\- #\+ #\!))
           (skipp (s)           (charn s  #\X)))
    (let (has xs ys (at -1))
      (loop for name in lst
        for at from 0 do
        (let* ((what (if (nump name) 'make-num 'make-sym))
               (col  (funcall what :at at :txt name)))
          (push col has)
          (when (not (skipp name))
            (if (goalp name) (push col ys) (push col xs)))))
      (%make-cols :has (reverse has) :xs xs :ys ys :names lst))))
;--------------------------------------------------------------------
(defun make-egs (&optional src &aux (self (%make-egs)))
  (typecase src
     (null   self)
     (cons   (dolist (row src self) (add self row)))
     (string (with-csv    (row src self) (add self row)))))

(defmethod add ((self egs) (r cons)) (add self (make-row :has r :within self)))
(defmethod add ((self egs) (r row))
  (with-slots (has cols) self
    (cond ((null cols)  (setf cols (make-cols (? r has))))
          (t            (setf (? r has) (mapcar 'add (? cols has) (? r has)))
                        (push r has)))))

(defmethod clone ((self egs) &optional inits)
  (make-egs (cons (? self cols names) inits)))

(defmethod mid ((self egs)) (mapcar 'mid (? self cols ys)))
;--------------------------------------------------------------------

 (let ((e (make-egs (!file my))))
   (setf (? e has ) (sort (? e has) #'lt))
   (let* ((n (length (? e has)))
          (n1 (floor (sqrt n))))
     (print (mapcar (lambda (c) (? c txt)) (? e cols ys)))
     (print (mapcar (lambda (c) (? c w)) (? e cols ys)))
     (print (rnds (mid (clone e (subseq (? e has) 1 n1)))))
     (print (rnds (mid (clone e (subseq (? e has) (- n n1))))))))
