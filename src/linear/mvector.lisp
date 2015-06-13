(in-package mobius-num.mvector)

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))

;; * M(athematical)VECTOR
;; Vector that can be either column (up indices) or a row (down
;; indices).

;; ** Utilities

(defun copy-1d-array (array)
  (let ((a (make-array (array-dimensions array)
                       :element-type (array-element-type array))))
    (loop for i from 0 below (array-dimension array 0)
       do (setf (aref a i) (aref array i)))
    a))

(defun copy-1d-array! (dest array)
  (loop for i from 0 below (array-dimension array 0)
     do (setf (aref dest i) (aref array i)))
  nil)

;; ** Definition
(defclass mvector ()
  ((datum :initarg :datum
          :accessor mvector-datum
          :documentation "Vector representation")
   (index-type :initarg :index-type
               :initform :up
               :accessor mvector-index-type
               :documentation "Type of index: up (column) or down (row)")))

;; ** Printing
(defmethod print-object ((obj mvector) out)
  (print-unreadable-object (obj out :type nil)
    (with-slots (datum index-type) obj
      (let ((datum-list (loop for x across datum collect x)))
        (format out "MVECTOR ~A ~{~A~^ ~}" index-type datum-list)))))

;; ** Constructors
;; *** Constructors from values
(defun mvector (index-type &rest vals)
  "Make mvector from vals"
  (make-instance 'mvector
                 :datum (make-array (length vals)
                                    :element-type 'double-float
                                    :initial-contents (mapcar #f(coerce % 'double-float)
                                                              vals))
                 :index-type index-type))

(defun up (&rest vals)
  "Make column mvector"
  (apply #'mvector :up vals))

(defun down (&rest vals)
  "Make row mvector"
  (apply #'mvector :down vals))

(defun array->mvector (index-type array)
  (let ((ns (array-dimensions array)))
    (if (and (null (cdr ns)) (eq (array-element-type array) 'double-float))
        (make-instance 'mvector
                       :datum array
                       :index-type index-type)
        (error "ARRAY->MVECTOR: incompatible ARRAY"))))

(defun array->up (array) (array->mvector :up array))
(defun array->down (array) (array->mvector :down array))

;; *** Const vectors
(defun const-mvector (index-type dim value)
  "Make mvector of size DIM and all values equal to VALUE"
  (make-instance 'mvector
                 :datum (make-array dim
                                    :element-type 'double-float
                                    :initial-element (coerce value 'double-float))
                 :index-type index-type))

(defun const-up (dim value)
  (const-mvector :up dim value))

(defun const-down (dim value)
  (const-mvector :down dim value))

(defun zero-mvector (index-type dim)
  (const-mvector index-type dim 0.0d0))

(defun zero-up (dim) (zero-mvector :up dim))
(defun zero-down (dim) (zero-mvector :down dim))

;; *** Compute vectors from functions

(defun build-mvector (index-type dim fun)
  "Build vector of size dim with function fun"
  (declare (type fixnum dim)
           (type function fun))
  (let ((v (zero-mvector index-type dim)))
    (loop for i from 0 below dim
       do (setf (aref (mvector-datum v) i) (funcall fun i))
       finally (return v))))

(defun build-up (dim fun) (build-mvector :up dim fun))
(defun build-down (dim fun) (build-mvector :down dim fun))

;; *** Delta vectors
;; These a the vectors that have all zeros, except for one index: it's
;; like a spike. Hence, similarity with delta-function.
(defun delta-mvector (index-type dim index value)
  "Computes a vector with all elements zero,
except for index, which has a provided value"
  (declare (type fixnum index)
           (type double-float value))
  (let ((v (zero-mvector index-type dim)))
    (setf (aref (mvector-datum v) index) value)
    v))

(defun delta-up (dim index value) (delta-mvector :up dim index value))
(defun delta-down (dim index value) (delta-mvector :down dim index value))


;; *** Copy constructor
(defun copy-mvector (mvector)
  "Creates an independent copy of MVECTOR"
  (make-instance 'mvector
                 :index-type (mvector-index-type mvector)
                 :datum (copy-1d-array (mvector-datum mvector))))

;; ** Referencing items

(declaim (inline mvref))

(defun mvref (v i)
  (declare (type mvector v)
           (type fixnum i))
  (aref (mvector-datum v) i))

(defun (setf mvref) (new-value v i)
  (declare (type mvector v)
           (type fixnum i))
  (setf (aref (mvector-datum v) i) (coerce new-value 'double-float)))

;; ** Shape Properties
;; *** Predicates and length
(defun mvector? (obj) (typep obj 'mvector))
(defun column? (v) (eq (mvector-index-type v) :up))
(defun up? (v) (column? v))
(defun row? (v) (eq (mvector-index-type v) :down))
(defun down? (v) (row? v))
(defun mvlength (v) (array-dimension (mvector-datum v) 0))

;; *** Compatibility check
(defun compatible? (u v)
  (and (or (and (up? u) (up? v))
           (and (down? u) (down? v)))
       (= (mvlength u) (mvlength v))))

(define-condition incompatible-mvector-size (error)
  ((error-message :initarg :message :reader error-message)))

(defun throw-incomaptible-mvector-size (op u v)
  (if (or (and (up? u) (up? v)) (and (down? u) (down? v)))
      (error 'incompatible-mvector-size
             :message (format nil "~A: incompatible vector sizes: expected ~A but given ~A"
                              op (mvector-index-type u) (mvector-index-type v)))
      (error 'incompatible-mvector-size
             :message (format nil "~A: incompatible lengthes: epxected ~D but given ~D"
                              op (mvlength u) (mvlength v)))))

;; *** Shape modifiers
(defun down-index (v)
  (make-instance 'mvector
                 :index-type :down
                 :datum (copy-1d-array (mvector-datum v))))

(defun up-index (v)
  (make-instance 'mvector
                 :index-type :up
                 :datum (copy-1d-array (mvector-datum v))))

(defun down-index! (v) (setf (mvector-index-type v) :down) nil)
(defun up-index! (v) (setf (mvector-index-type v) :up) nil)

(defmethod transpose ((v mvector))
  (if (up? v)
      (down-index v)
      (up-index v)))

;; ** More utility
(defun mvector-map (f v &rest vs)
  (let* ((n (mvlength v))
         (w (zero-mvector (mvector-index-type v) n)))
    (loop for i from 0 below n
       do (setf (mvref w i) (apply f (mvref v i) (mapcar #f(mvref % i) vs))))
    w))

(defun mvector-map! (f v &rest vs)
  (let* ((n (mvlength v)))
    (loop for i from 0 below n
       do (setf (mvref v i) (apply f (mvref v i) (mapcar #f(mvref % i) vs))))
    v))

(defmethod vector-dim ((u mvector)) (mvlength u))

;; ** Arithmetic operations
(defmacro define-mvector-methods-elt (op)
  "Constructs ELT-operations for MVECTOR, including destructive versions.
ELTx for (MVECTOR MVECTOR) (MVECTOR NUMBER) (NUMBER MVECTOR) (MVECTOR ARRAY) (ARRAY MVECTOR)
ELT=x! for (MVECTOR MVECTOR) (MVECTOR NUMBER) (MVECTOR ARRAY)
ELTx! for (MVECTOR MVECTOR MVECTOR) (MVECTOR MVECTOR NUMBER) (MVECTOR NUMBER MVECTOR)"
  (let ((pure-op (intern (format nil "ELT~A" op)))
        (assign-op (intern (format nil "ELT=~A!" op)))
        (dest-op (intern (format nil "ELT~A!" op)))
        (op-name (symbol-name op)))
    (with-gensyms (u v w dest i x y z)
      `(progn
         (defmethod ,pure-op ((,u mvector) (,v mvector))
           (if (compatible? ,u ,v)
               (mvector-map #',op ,u ,v)
               (throw-incomaptible-mvector-size ,op-name ,u ,v)))
         (defmethod ,pure-op ((,u mvector) (,v number))
           (mvector-map #f(,op % v) ,u))
         (defmethod ,pure-op ((,u number) (,v mvector))
           (mvector-map #f(,op ,u %) ,v))
         (defmethod ,pure-op ((,u mvector) (,v array))
           (if (equal (array-dimensions (mvector-datum ,u))
                      (array-dimensions ,v))
               (let ((,w (zero-mvector (mvector-index-type ,u) (mvlength ,u))))
                 (loop for ,i below (mvlength ,u)
                    do (setf (mvref ,v ,i) (,op (mvref ,u i) (aref ,v i))))
                 ,w)))
         (defmethod ,pure-op ((,v array) (,u mvector))
           (if (equal (array-dimensions (mvector-datum ,u))
                      (array-dimensions ,v))
               (let ((,w (zero-mvector (mvector-index-type ,u) (mvlength ,u))))
                 (loop for ,i below (mvlength ,u)
                    do (setf (mvref ,v ,i) (,op (mvref ,u i) (aref ,v i))))
                 ,w)))
         (defmethod ,assign-op ((,u mvector) (,v mvector))
           (if (compatible? ,u ,v)
               (mvector-map! #',op ,u ,v)
               (throw-incomaptible-mvector-size ,op-name ,u ,v)))
         (defmethod ,assign-op ((,u mvector) (,v number))
           (mvector-map! #f(,op % ,v) ,u))
         (defmethod ,assign-op ((,u mvector) (,v array))
           (if (equal (array-dimensions (mvector-datum ,u))
                      (array-dimensions ,v))
               (loop for i below (mvlength ,u)
                  do (setf (mvref ,u i) (,op (mvref ,u i) (aref ,v i)))
                  finally (return ,u))
               (throw-incomaptible-mvector-size ,op-name ,u ,v)))
         (defmethod ,dest-op ((,dest mvector) (,u mvector) (,v mvector))
           (if (and (compatible? ,dest ,u) (compatible? ,u ,v))
               (mvector-map! (lambda (,x ,y ,z)
                               (declare (ignore ,x))
                               (,op ,y ,z))
                             ,dest
                             ,u
                             ,v)
               (throw-incomaptible-mvector-size ,op ,u ,v)))
         (defmethod ,dest-op ((,dest mvector) (,u mvector) (,v number))
           (if (compatible? ,dest ,u)
               (mvector-map! (lambda (,x ,y)
                               (declare (ignore ,x))
                               (,op ,y ,v))
                             ,dest
                             ,u)
               (throw-incomaptible-mvector-size ,op ,dest ,u)))
         (defmethod ,dest-op ((,dest mvector) (,v number) (,u mvector))
           (if (compatible? ,dest ,u)
               (mvector-map! (lambda (,x ,y)
                               (declare (ignore ,x))
                               (,op ,y ,v))
                             ,dest
                             ,u)
               (throw-incomaptible-mvector-size ,op ,dest ,u)))))))

(define-mvector-methods-elt +)
(define-mvector-methods-elt -)
(define-mvector-methods-elt *)
(define-mvector-methods-elt /)

(defmethod elt=-rev/! ((u mvector) (v mvector))
  (if (compatible? u v)
      (mvector-map! (lambda (x y) (/ y x)) u v)))

(defmethod elt=-rev/! ((u mvector) (v number))
  (mvector-map! #f(/ v %) u))


(defmethod elt-negate ((u mvector))
  (mvector-map #'- u))

(defmethod elt-negate! ((u mvector))
  (mvector-map! #'- u)
  u)

(defmethod elt-zero ((u mvector))
  (zero-mvector (mvector-index-type u) (mvlength u)))

(defmethod elt-zero! ((u mvector))
  (mvector-map! (lambda (x) (declare (ignore x)) 0.0d0) u))


;; ** Numeric properties

(defmethod norm ((v mvector))
  (case *norm-type*
    (nil (loop for x across (mvector-datum v) maximizing (abs x)))
    (1 (loop for x across (mvector-datum v) summing (abs x)))
    (2 (sqrt (loop for x across (mvector-datum v) summing (* x x))))
    (otherwise (expt (loop for x across (mvector-datum v)
                        summing (expt x *norm-type*))
                     (/ *norm-type*)))))

(defmethod num= ((u mvector) (v mvector) &optional eps)
  (num= (norm (elt- u v)) 0.0d0 (or eps *num=-tolerance*)))

(defmethod m* ((u mvector) (v mvector) &optional destination)
  (cond ((and (down? u) (up? v))                                       ; inner-product
         (let ((n (mvlength u)))
           (if (= n (mvlength v))
               (loop for i from 0 below n
                  summing (* (mvref u i) (mvref v i)))
               (error 'incompatible-mvector-size
                      :message (format nil "MM: vectors have different lengthes")))))
        ((and (up? u) (down? v))                                      ; outer product
         (let* ((n (mvlength u))
                (m (mvlength v))
                (w (or destination
                       (make-array (list n m) :element-type 'double-float))))
           (loop for i from 0 below n
              do (loop for j from 0 below m
                    do (setf (aref w i j) (* (mvref u i) (mvref v j)))))
           w))
        (t (if (= (mvlength u) (mvlength v) 1)
               (mvector (mvector-index-type u) (* (mvref u 0) (mvref v 0)))
               (error 'incompatible-mvector-size
                      :message (format nil "MM: vectors have incompatible index types"))))))

(defmethod m* ((u array) (v mvector) &optional destination)
  (cond ((and (down? v) (vectorp u)) (outer-product u (mvector-datum v) destination))
        ((and (vectorp u) (up? v)) (dot u (mvector-datum v)))
        ((= (array-rank u) 2)
         (let ((dest (or destination (elt-zero v))))
           (setf (mvector-datum dest)
                 (m* u (mvector-datum v) (mvector-datum dest)))
           dest))
        (t (m* u (mvector-datum v) destination))))

(defmethod mm ((u mvector) (v mvector)) (m* u v))

(defmethod dot ((u mvector) (v mvector))
  "Computes inner-product of two MVECTORs regardless their index types"
  (let ((n (mvlength u)))
    (if (= n (mvlength v))
        (loop for i from 0 below n
           summing (* (mvref u i) (mvref v i)))
        (error 'incompatible-mvector-size
               :message (format nil "DOT: vectors have different lengthes")))))

(defmethod outer-product ((u mvector) (v mvector) &optional buffer)
  (outer-product (mvector-datum u) (mvector-datum v) buffer))

(defmethod lla:solve ((A t) (B mvector))
  (array->mvector (mvector-index-type B) (lla:solve A (mvector-datum B))))

(defmethod m/ ((a array) (b mvector) &optional (x0 (copy-mvector b)) x)
  "Uses BICGSTAB"
  (declare (ignore x))
  (let ((y (mobius-num.bicg-stab:bicgstab a b x0)))
    (cond ((iterator:finished? y)
           (car (iterator:value y)))
          (t (error "M/: BICGSTAB failed to converge for matrix ~A and vector ~A"
                    a b)))))
