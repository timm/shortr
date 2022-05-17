;vim: ts=2 sw=2 et :
(defstruct cntrl
  (p 2))

(defparameter my (make-cntrl))

(defun get-file (file) 
  (with-open-file (str file) (read str nil)))

(defvar *e*   (exp 1))x
(defvar *big* most-positive-fixnum)

(labels ((chars (s) (if (stringp s) s (symbol-name s)))
         (chrn  (x) (let ((s (chars x))) (char s (1- (length s)))))
         (chr0  (x) (char (chars x) 0)))
  (defun showp(x) (not (eq      (chr0 x) #\_)))
  (defun skipp(x) (eql          (chrn x) '#\X))
  (defun lessp(x) (eql          (chrn x) '#\-))
  (defun goalp(x) (member       (chrn x) '(#\- #\+ #\!)))
  (defun nump (x) (upper-case-p (chr0 x))))

(defmacro ?  (p x &rest xs)
  (if (null xs) `(slot-value ,p ',x) `(? (slot-value ,p ',x) ,@xs)))

(defun no (x) (eq '? x))

(defmacro has (x a)
   `(cdr (or (assoc ,x ,a :test #'equal)
             (car (setf ,a (cons (cons ,x 0) ,a))))))

(defmacro defthing (x &rest lst) 
  (let ((it (gensym)))
    `(progn 
       (defstruct (,x (:include thing)
                      (:constructor ,(intern (format nil "%~@:(~a~)" x)))) ,@lst)
       (defmethod slots ((,it ,x)) 
         ',(remove-if-not 'showp (sort (mapcar 'first lst) 'string<))))))

(defstruct thing)
(defmethod print-object ((x thing) str) 
  (labels ((fun (y) (cons y (slot-value x y))))
    (format str "~a" (cons (type-of x) (mapcar #'fun (slots x))))))

(defun nasa93dem() 
  '((idX centerX Year prec flex resl team pmat rely data cplx 
         ruse docu time stor pvol acap pcap pcon 
         apex plex ltex tool site sced Kloc 
         Effort- Defects- Months-) 
    (1 2 1979 h h h vh h h l h n n n n l n n n n n h n n l 25.9 117.6 808 15.3) 
    (2 2 1979 h h h vh h h l h n n n n l n n n n n h n n l 24.6 117.6 767 15) 
    (3 2 1979 h h h vh h h l h n n n n l n n n n n h n n l 7.7 31.2 240 10.1) 
    (4 2 1979 h h h vh h h l h n n n n l n n n n n h n n l 8.2 36 256 10.4) 
    (5 2 1979 h h h vh h h l h n n n n l n n n n n h n n l 9.7 25.2 302 11) 
    (6 2 1979 h h h vh h h l h n n n n l n n n n n h n n l 2.2 8.4 69 6.6) 
    (7 2 1979 h h h vh h h l h n n n n l n n n n n h n n l 3.5 10.8 109 7.8) 
    (8 2 1982 h h h vh h h l h n n n n l n n n n n h n n l 66.6 352.8 2077 21) 
    (9 1 1980 h h h vh h h l h n n xh xh l h h n h n h h n n 7.5 72 226 13.6) 
    (10 1 1980 h h h vh n n l h n n n n l h vh n vh n h n n n 20 72 566 14.4) 
    (11 1 1984 h h h vh n n l h n n n n l h h n vh n h n n n 6 24 188 9.9) 
    (12 1 1980 h h h vh n n l h n n n n l h vh n vh n h n n n 100 360 2832 25.2) 
    (13 1 1985 h h h vh n n l h n n n n l h n n vh n l n n n 11.3 36 456 12.8) 
    (14 1 1980 h h h vh n n l h n n n n h h h n h l vl n n n 100 215 5434 30.1) 
    (15 1 1983 h h h vh n n l h n n n n l h h n vh n h n n n 20 48 626 15.1) 
    (16 1 1982 h h h vh n n l h n n n n l h n n n n vl n n n 100 360 4342 28) 
    (17 1 1980 h h h vh n n l h n n n xh l h vh n vh n h n n n 150 324 4868 32.5) 
    (18 1 1984 h h h vh n n l h n n n n l h h n h n h n n n 31.5 60 986 17.6) 
    (19 1 1983 h h h vh n n l h n n n n l h h n vh n h n n n 15 48 470 13.6) 
    (20 1 1984 h h h vh n n l h n n n xh l h n n h n h n n n 32.5 60 1276 20.8) 
    (21 2 1985 h h h vh h h l h n n n n l n n n n n h n n l 19.7 60 614 13.9) 
    (22 2 1985 h h h vh h h l h n n n n l n n n n n h n n l 66.6 300 2077 21) 
    (23 2 1985 h h h vh h h l h n n n n l n n n n n h n n l 29.5 120 920 16) 
    (24 2 1986 h h h vh n h n n n n h n n n h n h n n n n n 15 90 575 15.2) 
    (25 2 1986 h h h vh n h n h n n n n n n h n h n n n n n 38 210 1553 21.3) 
    (26 2 1986 h h h vh n n n n n n n n n n h n h n n n n n 10 48 427 12.4) 
    (27 2 1982 h h h vh h n vh h n n vh vh l vh n n h l h n n l 15.4 70 765 14.5) 
    (28 2 1982 h h h vh h n vh h n n vh vh l vh n n h l h n n l 48.5 239 2409 21.4) 
    (29 2 1982 h h h vh h n vh h n n vh vh l vh n n h l h n n l 16.3 82 810 14.8) 
    (29 2 1982 h h h vh h n vh h n n vh vh l vh n n h l h n n l 12.8 62 636 13.6) 
    (31 2 1982 h h h vh h n vh h n n vh vh l vh n n h l h n n l 32.6 170 1619 18.7) 
    (32 2 1982 h h h vh h n vh h n n vh vh l vh n n h l h n n l 35.5 192 1763 19.3) 
    (33 2 1985 h h h vh h h l h n n n n l n n n n n h n n l 5.5 18 172 9.1) 
    (34 2 1987 h h h vh h h l h n n n n l n n n n n h n n l 10.4 50 324 11.2) 
    (35 2 1987 h h h vh h h l h n n n n l n n n n n h n n l 14 60 437 12.4) 
    (36 2 1986 h h h vh n h n h n n n n n n n n n n n n n n 6.5 42 290 12) 
    (37 2 1986 h h h vh n n n h n n n n n n n n n n n n n n 13 60 683 14.8) 
    (38 2 1986 h h h vh h n n h n n n n n n h n n n h h n n 90 444 3343 26.7) 
    (39 2 1986 h h h vh n n n h n n n n n n n n n n n n n n 8 42 420 12.5) 
    (40 2 1986 h h h vh n n n h n n h n n n n n n n n n n n 16 114 887 16.4) 
    (41 2 1980 h h h vh h n h h n n vh h l h h n n l h n n l 177.9 1248 7998 31.5) 
    (42 6 1975 h h h vh h h l h n n n n l n h n n n n n n n 302 2400 8543 38.4) 
    (43 5 1982 h h h vh h n h l n n n n h h n n h n n h n n 282.1 1368 9820 37.3) 
    (44 5 1982 h h h vh h h h l n n n n n h n n h n n n n n 284.7 973 8518 38.1) 
    (45 5 1982 h h h vh n h h n n n n n l n h n h n h n n n 79 400 2327 26.9) 
    (46 5 1977 h h h vh l l n n n n n n l h vh n h n h n n n 423 2400 18447 41.9) 
    (47 5 1977 h h h vh h n n n n n n n l h vh n vh l h n n n 190 420 5092 30.3) 
    (48 5 1984 h h h vh h n n h n n n h n h n n h n h n n n 47.5 252 2007 22.3) 
    (49 5 1980 h h h vh l vh n xh n n h h l n n n h n n h n n 21 107 1058 21.3) 
    (50 5 1983 h h h vh l n h h n n vh n n h h n h n h n n n 78 571.4 4815 30.5) 
    (51 5 1984 h h h vh l n h h n n vh n n h h n h n h n n n 11.4 98.8 704 15.5) 
    (52 5 1985 h h h vh l n h h n n vh n n h h n h n h n n n 19.3 155 1191 18.6) 
    (53 5 1979 h h h vh l h n vh n n h h l h n n n h h n n n 101 750 4840 32.4) 
    (54 5 1979 h h h vh l h n h n n h h l n n n h n n n n n 219 2120 11761 42.8) 
    (55 5 1979 h h h vh l h n h n n h h l n n n h n n n n n 50 370 2685 25.4) 
    (56 2 1979 h h h vh h vh h h n n vh vh n vh vh n vh n h h n l 227 1181 6293 33) 
    (57 2 1977 h h h vh h n h vh n n n n l h vh n n l n n n l 70 278 2950 20.2) 
    (58 2 1979 h h h vh h h l h n n n n l n n n n n h n n l 0.9 8.4 28 4.9) 
    (59 6 1974 h h h vh l vh l xh n n xh vh l h h n vh vl h n n n 980 4560 50961 96) 
    (60 6 1975 h h h vh n n l h n n n n l vh vh n n h h n n n 350 720 8547 35.7) 
    (61 5 1976 h h h vh h h n xh n n h h l h n n n h h h n n 70 458 2404 27.5) 
    (62 5 1979 h h h vh h h n xh n n h h l h n n n h h h n n 271 2460 9308 43.4) 
    (63 5 1971 h h h vh n n n n n n n n l h h n h n h n n n 90 162 2743 25) 
    (64 5 1980 h h h vh n n n n n n n n l h h n h n h n n n 40 150 1219 18.9) 
    (65 5 1979 h h h vh n h n h n n h n l h h n h n h n n n 137 636 4210 32.2) 
    (66 5 1977 h h h vh n h n h n n h n h h h n h n h n n n 150 882 5848 36.2) 
    (67 5 1976 h h h vh n vh n h n n h n l h h n h n h n n n 339 444 8477 45.9) 
    (68 5 1983 h h h vh n l h l n n n n h h h n h n h n n n 240 192 10313 37.1) 
    (69 5 1978 h h h vh l h n h n n n vh l h h n h h h n n l 144 576 6129 28.8) 
    (70 5 1979 h h h vh l n l n n n n vh l h h n h h h n n l 151 432 6136 26.2) 
    (71 5 1979 h h h vh l n l h n n n vh l h h n h h h n n l 34 72 1555 16.2) 
    (72 5 1979 h h h vh l n n h n n n vh l h h n h h h n n l 98 300 4907 24.4) 
    (73 5 1979 h h h vh l n n h n n n vh l h h n h h h n n l 85 300 4256 23.2) 
    (74 5 1982 h h h vh l n l n n n n vh l h h n h h h n n l 20 240 813 12.8) 
    (75 5 1978 h h h vh l n l n n n n vh l h h n h h h n n l 111 600 4511 23.5) 
    (76 5 1978 h h h vh l h vh h n n n vh l h h n h h h n n l 162 756 7553 32.4) 
    (77 5 1978 h h h vh l h h vh n n n vh l h h n h h h n n l 352 1200 17597 42.9) 
    (78 5 1979 h h h vh l h n vh n n n vh l h h n h h h n n l 165 97 7867 31.5) 
    (79 5 1984 h h h vh h h n vh n n h h l h n n n h h n n n 60 409 2004 24.9) 
    (80 5 1984 h h h vh h h n vh n n h h l h n n n h h n n n 100 703 3340 29.6) 
    (81 2 1980 h h h vh n h vh vh n n xh xh h n n n n l l n n n 32 1350 2984 33.6) 
    (82 2 1980 h h h vh h h h h n n vh xh h h h n h h h n n n 53 480 2227 28.8) 
    (83 3 1977 h h h vh h h l vh n n vh xh l vh vh n vh vl vl h n n 41 599 1594 23) 
    (84 3 1977 h h h vh h h l vh n n vh xh l vh vh n vh vl vl h n n 24 430 933 19.2) 
    (85 5 1977 h h h vh h vh h vh n n xh xh n h h n h h h n n n 165 4178.2 6266 47) 
    (86 5 1977 h h h vh h vh h vh n n xh xh n h h n h h h n n n 65 1772.5 2468 34.5) 
    (87 5 1977 h h h vh h vh h vh n n xh xh n h h n h h h n n n 70 1645.9 2658 35.4) 
    (88 5 1977 h h h vh h vh h xh n n xh xh n h h n h h h n n n 50 1924.5 2102 34.2) 
    (89 5 1982 h h h vh l vh l vh n n vh xh l h n n l vl l h n n 7.25 648 406 15.6) 
    (90 5 1980 h h h vh h vh h vh n n xh xh n h h n h h h n n n 233 8211 8848 53.1) 
    (91 2 1983 h h h vh n h n vh n n vh vh h n n n n l l n n n 16.3 480 1253 21.5) 
    (92 2 1983 h h h vh n h n vh n n vh vh h n n n n l l n n n 6.2 12 477 15.4) 
    (93 2 1983 h h h vh n h n vh n n vh vh h n n n n l l n n n 3 38 231 12)))

;-------------------------------------------------------------------------------
(defthing num 
  (n 0) (at) (txt) (mu 0) (m2 0) (sd 0) (w 1) (lo *big*) (hi (* -1 *big*)))

(defun make-num (&optional (at 0) (txt " ") (self (%num :at at :txt txt)))
  (if (lessp txt) (setf (num-w self) -1))
  self)

(defmethod add ((self num) x)
 (with-slots (n mu m2 sd lo hi) self
  (unless (no x) 
    (let ((d (- x mu)))
      (incf n) 
      (incf mu (/ d n))
      (incf m2 (* d (- x mu)))
      (setf sd (if (or  (< m2 0) (< n 2)) 0 (sqrt (/ m2 (- n 1))))
            lo (min x lo)
            hi (max x hi))))))

(defmethod norm ((self num) x) 
  (with-slots (lo hi) self
    (if  (< (- hi lo) 1E9) 0 (/ (- x lo) (- hi lo (/ -1 *big*))))))

(defmethod dist ((self num) x y)
  (cond ((and (no x) (no y)) 0)
        ((no x) (setf y (norm self y) x (if (< y .5) 1 0)))
        ((no y) (setf x (norm self x) y (if (< x .5) 1 0)))
        (t      (setf x (norm self x) y (norm self y))))
  (abs (- x y)))

(defmethod like ((self num) x prior)
  (declare (ignore prior))
  (with-slots (mu sd) self 
    (cond ((< x (- mu (* 4 sd))) 0)
          ((> x (+ mu (* 4 sd))) 0)
          (t (/ (exp (- (/ (expt (- x mu) 2) (* 2 (expt sd 2)))))
                (* sd (sqrt (* 2 pi))))))))

(defmethod mid ((self num)) (num-mu self))

(defmethod range ((self num) x n)
  (with-slots (lo hi) self
    (let ((b (/ (- hi lo) n))) (* b (floor (+ (/ x b) .5))))))
;-------------------------------------------------------------------------------
(defthing sym
  (n 0) (at) (txt) (all))

(defun make-sym (&optional (at 0) (txt " ")(it (%sym :at at :txt txt)))
  it)

(defmethod add ((self sym) x)
  (with-slots (n all ) self 
    (unless (no x) (incf n) (incf (has x all)))))

(defmethod dist ((self sym) x y)
  (if (equal x y) 0 1))

(defmethod div ((self sym))
  (with-slots (n all) self
    (reduce '- (loop for (_ . v) in all collect (* (/ v n) (log (/ v n) 2))))))

(defmethod like ((self sym) x prior)
  (with-slots (all n) self
  (let ((n (or (cdr (assoc x all) 0))))
    (/ (+ n (* prior (? my m))) (
  
   

(defmethod adds (self lst) (dolist (one lst self) (add self one)))

(print (adds (make-num) '(10 1 20 3 30)))
(print (adds (make-sym) '(a a a a b b c)))


; function SYM.like(i,x,prior) return ((c.all[x] or 0)+the.m*prior)/(c.n+the.m) end
;
; function SYM.merge(i,ranges,min) return ranges end
;
; function SYM.mid(i)
;   m=0; for y,n in pairs(i.all) do if n>m then m,x=n,y end end; return x end
;
; function SYM.range(i,x,_) return x end
; -
