(in-package mobius-num.linear-operations-arrays)

;; * Implementation of linear operations for arrays
;; ** Utility for arrays
(defun operate-on-arrays (op c a &rest b)
  "Apply elementwise function OP to elements of arrays A and B, saving the result in C"
  (let ((a-dims (array-dimensions a))
        (c-dims (array-dimensions c)))
    (if (equal a-dims c-dims)
        (labels ((process-dimensions (dims inds)
                   (if (null dims)
                       (let ((inds (reverse inds)))
                         (setf (apply #'aref c inds)
                               (apply op
                                      (apply #'aref a inds)
                                      (mapcar #f(apply #'aref % inds)
                                              b))))
                       (let ((n (car dims))
                             (other-dims (cdr dims)))
                         (loop for i from 0 below n
                            do (process-dimensions other-dims (cons i inds)))))))
          (process-dimensions a-dims nil))
        (error (format nil "OPERATE-ON-ARRAYS: arrays sizes do not match!")))))

;; ** Shape
(defmethod vector-dim ((u vector))
  (array-dimension u 0))

(defmethod transpose ((u vector))
  u)

;; ** Norm
(defmethod norm ((v vector))
  (case *NORM-TYPE*
    (NIL (loop for x across v maximizing (abs x)))
    (1 (loop for x across v summing (abs x)))
    (2 (sqrt (loop for x across v summing (* x x))))
    (t (expt (loop for x across v summing (expt (abs x) *NORM-TYPE*))
             (/ *NORM-TYPE*)))))

;; ** Negate
(defmethod elt-negate ((u vector))
  (let ((v (make-array (array-dimensions u)
                       :element-type (array-element-type u))))
    (loop for i below (array-dimension u 0)
       do (setf (aref v i) (- (aref u i))))
    v))

(defmethod elt-negate! ((u vector))
  (loop for i below (array-dimension u 0)
     do (setf (aref u i) (- (aref u i))))
  u)

(defmethod elt-negate ((u array))
  (let ((w (make-array (array-dimensions u)
                       :element-type (array-element-type u))))
    (operate-on-arrays #'- w u)
    w))

(defmethod elt-negate! ((u array))
  (operate-on-arrays #'- u u)
  u)

;; ** Zero
(defmethod elt-zero ((u array))
  (let ((v (make-array (array-dimensions u)
                       :element-type (array-element-type u)
                       :initial-element 0)))
    v))

(defmethod elt-zero! ((u array))
  (operate-on-arrays (lambda (&rest args)
                       (declare (ignore args))
                       0)
                     u
                     u))

;; ** Arithmetic

(defmacro define-array-methods-elt (primitive-op)
  (let ((pure-op (intern (format nil "ELT~A" primitive-op)))
        (assign-op (intern (format nil "ELT=~A!" primitive-op)))
        (dest-op (intern (format nil "ELT~A!" primitive-op))))
    (with-gensyms (u v w)
      `(progn
         (defmethod ,pure-op ((,u array) (,v array))
           (let ((,w (make-array (array-dimensions ,u)
                                 :element-type (array-element-type ,u))))
             (operate-on-arrays #',primitive-op ,w ,u ,v)
             ,w))
         (defmethod ,assign-op ((,u array) (,v array))
           (operate-on-arrays #',primitive-op ,u ,u ,v)
           ,u)
         (defmethod ,dest-op ((,w array) (,u array) (,v array))
           (operate-on-arrays #',primitive-op ,w ,u ,v)
           ,w)
         (defmethod ,pure-op ((,u array) (,v number))
           (let ((,w (make-array (array-dimensions ,u)
                                 :element-type (array-element-type ,u))))
             (operate-on-arrays #f(,primitive-op % ,v) ,w ,u)
             ,w))
         (defmethod ,pure-op ((,u number) (,v array))
           (let ((,w (make-array (array-dimensions ,v)
                                 :element-type (array-element-type ,v))))
             (operate-on-arrays #f(,primitive-op % ,u) ,w ,v)
             ,w))
         (defmethod ,assign-op ((,u array) (,v number))
           (operate-on-arrays #f(,primitive-op % ,v) ,u ,u)
           ,u)
         (defmethod ,dest-op ((,w array) (,u array) (,v number))
           (operate-on-arrays #f(,primitive-op % ,v) ,w ,u)
           ,w)))))

(define-array-methods-elt +)
(define-array-methods-elt -)
(define-array-methods-elt *)
(define-array-methods-elt /)

(defmethod elt=-rev/! ((u array) (v array))
  (operate-on-arrays (lambda (eu ev) (/ ev eu)) u u v)
  u)

(defmethod elt=-rev/! ((u array) (v number))
  (operate-on-arrays #f(/ v %) u u)
  u)

;; ** Structured multiplications
;; for vectors (simpler implementation)
(defmethod m* ((u vector) (v vector) &optional destination)
  (declare (ignore destination))
  (let ((u-dim (array-dimension u 0))
        (v-dim (array-dimension v 0)))
    (if (= u-dim v-dim)
        (loop for i below u-dim
           summing (* (aref u i) (aref v i)))
        (error "M* (VECTOR VECTOR): dimensions do not match"))))
;; for multidimensional arrays
(defmethod m* ((u array) (v array) &optional destination)
  (let* ((u-dims (array-dimensions u))
         (v-dims (array-dimensions v))
         (u-n (car (last u-dims)))
         (v-n (car v-dims)))
    (if (= u-n v-n)
        (let ((x (or destination (make-array (append (butlast u-dims) (cdr v-dims))
                                             :element-type (array-element-type u)))))
          (labels ((process-a (dims1 dims2 inds1 inds2)
                     (cond ((and (null dims1) (null dims2))
                            (setf (apply #'aref
                                         x
                                         (nconc (reverse inds1)
                                                (reverse inds2)))
                                  (loop for i from 0 below u-n
                                     summing (* (apply #'aref
                                                       u
                                                       (reverse (cons i inds1)))
                                                (apply #'aref
                                                       v
                                                       (cons i (reverse inds2)))))))
                           ((null dims1)                                              ; exhausted u-indices, loop over v-inidices
                            (loop for i from 0 below (car dims2)                      ; dims2 is not null at this point
                               do (process-a nil
                                             (cdr dims2)
                                             inds1
                                             (cons i inds2))))
                           (t (loop for i from 0 below (car dims1)
                                 do (process-a (cdr dims1) dims2 (cons i inds1) inds2))))
                     ))
            (process-a (butlast u-dims) (cdr v-dims) nil nil)
            x))
        (error "M* (ARRAY ARRAY): inner dimensions do not match"))))
;; DOT product for vectors is just M*
(defmethod dot ((u vector) (v vector))
  (m* u v))
;; OUTER-PRODUCT for two vectors
(defmethod outer-product ((u vector) (v vector) &optional buffer)
  (let* ((n (array-dimension u 0))
         (m (array-dimension v 0))
         (a (or buffer (make-array (list n m)
                                   :element-type (array-element-type u)))))
    (loop for i below n
       do (loop for j below m
             do (setf (aref a i j) (* (aref u i) (aref v j)))))
    a))

;; ** Structured division
(defmethod m/ ((a array) (b vector) &optional x0 dest)
  "Implementation using LLA:SOLVE"
  (declare (ignore x0 dest))
  (lla:solve a b))
