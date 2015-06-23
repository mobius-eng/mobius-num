(in-package mobius.numeric.mvector)

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))

;; * M(athematical)VECTOR
;; Vector that can be either column (up indices) or a row (down
;; indices).

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
                 :datum (duplicate-vector (mvector-datum mvector))))

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
(defun mvector-p (obj) (typep obj 'mvector))
(defun column-p (v) (eq (mvector-index-type v) :up))
(defun up-p (v) (column-p v))
(defun row-p (v) (eq (mvector-index-type v) :down))
(defun down-p (v) (row-p v))
(defun mvlength (v) (array-dimension (mvector-datum v) 0))

;; *** Compatibility check
(defun compatible-p (u v)
  (and (or (and (up-p u) (up-p v))
           (and (down-p u) (down-p v)))
       (= (mvlength u) (mvlength v))))

(define-condition incompatible-mvector-size (error)
  ((error-message :initarg :message :reader error-message)))

(defun throw-incomaptible-mvector-size (op u v)
  (if (or (and (up-p u) (up-p v)) (and (down-p u) (down-p v)))
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
                 :datum (duplicate-vector (mvector-datum v))))

(defun up-index (v)
  (make-instance 'mvector
                 :index-type :up
                 :datum (duplicate-vector (mvector-datum v))))

(defun down-index! (v) (setf (mvector-index-type v) :down) nil)
(defun up-index! (v) (setf (mvector-index-type v) :up) nil)

;; ** Generics implementations
;; *** Constructors
(defmethod zero-vector ((original-vector mvector) &optional destination)
  (cond ((null destination) (zero-mvector (mvector-index-type original-vector)
                                          (mvlength original-vector)))
        ((vectorp destination) (array->mvector (mvector-index-type original-vector)
                                               (map-vector destination
                                                           (constantly 0.0d0)
                                                           :destination destination)))
        ((mvector-p destination) (map-vector destination
                                             (constantly 0.0d0)
                                             :destination destination))
        (t (error "ZERO-VECTOR (MVECTOR): unknown destination type ~A"
                  (type-of destination)))))

(defmethod make-vector ((original-vector mvector) dim)
  (zero-mvector (mvector-index-type original-vector) dim))

(defmethod list->vector-method ((vector-type (eql 'mvector)) list)
  (apply #'up list))

;; *** Shape
(defmethod vector-dim ((u mvector)) (mvlength u))

(defmethod transpose ((v mvector))
  (if (up-p v)
      (down-index v)
      (up-index v)))

;; *** Mapping
(defmethod map-vector ((vector mvector) function
                       &key destination other-vectors with-indices)
  (let ((destination (cond ((null destination) (zero-vector vector))
                           ((vectorp destination) (array->mvector
                                                   (mvector-index-type vector)
                                                   destination))
                           ((mvector-p destination) destination)
                           (t (error "MAP-VECTOR(MVECTOR): unnknown destination type ~A"
                                     destination))))
        (other-vectors (mapcar #'(lambda (v)
                                   (cond ((mvector-p v) (slot-value v 'datum))
                                         ((vectorp v) v)
                                         (t (error "MAP-VECTOR (MVECTOR): wrong type of OTHER-VECTORS vector ~A"
                                                   (type-of v)))))
                               other-vectors)))
    (declare (type mvector destination))
    (setf (slot-value destination 'datum)
          (map-vector (slot-value vector 'datum)
                      function
                      :with-indices with-indices
                      :destination (slot-value destination 'datum)
                      :other-vectors other-vectors))
    destination))

;; *** Reduction
(defmethod reduce-vector ((vector mvector) reducing-function initial-value
                          &key mapping-function other-vectors)
  (reduce-vector (slot-value vector 'datum)
                 reducing-function
                 initial-value
                 :mapping-function mapping-function
                 :other-vectors (mapcar #'mvector-datum other-vectors)))

;; ** Numeric properties

(defmethod num= ((u mvector) (v mvector) &optional eps)
  (num= (norm (.- u v)) 0.0d0 (or eps *num=-tolerance*)))

(defmethod m* ((u mvector) (v mvector) &optional destination)
  (cond ((and (down-p u) (up-p v))                                      ; inner-product
         (let ((n (mvlength u)))
           (if (= n (mvlength v))
               (loop for i from 0 below n
                  summing (* (mvref u i) (mvref v i)))
               (error 'incompatible-mvector-size
                      :message (format nil "MM: vectors have different lengthes")))))
        ((and (up-p u) (down-p v))                                      ; outer product
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
  (cond ((and (down-p v) (vectorp u)) (outer-product u (mvector-datum v) destination))
        ((and (vectorp u) (up-p v)) (dot u (mvector-datum v)))
        ((= (array-rank u) 2)
         (let ((dest (or destination (zero-vector v))))
           (setf (mvector-datum dest)
                 (m* u (mvector-datum v) (mvector-datum dest)))
           dest))
        (t (m* u (mvector-datum v) destination))))

(defmethod mm ((u mvector) (v mvector)) (m* u v))

(defmethod outer-product ((u mvector) (v mvector) &optional buffer)
  (outer-product (mvector-datum u) (mvector-datum v) buffer))

(defmethod inner-product ((u mvector) (v mvector))
  (m* u v))

(defmethod lla:solve ((A t) (B mvector))
  (array->mvector (mvector-index-type B) (lla:solve A (mvector-datum B))))

;; (defmethod m/ ((a array) (b mvector) &optional (x0 (copy-mvector b)))
;;   "Uses BICGSTAB"
;;   (declare (ignore x))
;;   (let ((y (bicgstab a b x0)))
;;     (cond ((iterator:finished? y)
;;            (car (iterator:value y)))
;;           (t (error "M/: BICGSTAB failed to converge for matrix ~A and vector ~A"
;;                     a b)))))
