(in-package mobius.numeric.tests.linear)

(def-suite linear-suite
    :description "Testing all the functions from LINEAR modules")

(in-suite linear-suite)

(test linear-numbers
  "Implementation of linear operations for numbers"
  (is (= (zero-vector 1.0d0) 0.0d0))
  (is (numberp (make-vector 1.0d0 0)))
  (is (= (vector-dim 1.0d0) 0))
  (is (num= (map-vector 2.0d0 #f(+ % 3.0d0)) 5.0d0))
  (is (num= (map-vector! 2.0d0 #f(+ % 3.0d0)) 5.0d0))
  (is (num= (reduce-vector 1.0d0 #'(lambda (r x) (+ r x)) 10.0d0) 11.0d0))
  (is (num= (dot 2.0d0 3.0d0) 6.0d0))
  (is (num= (m* 2.0d0 3.0d0) 6.0d0))
  (is (num= (m/ 6.0d0 3.0d0) (/ 3.0d0 6.0d0)))
  (is (num= (transpose 2.0d0) 2.0d0)))

(test linear-vectors
  (is (num= (zero-vector #(1 2 3)) #(0 0 0)))
  (let ((v #(1 2 3)))
    (is (num= (zero-vector! v) #(0 0 0)))
    (is (num= v #(0 0 0))))
  (is (vectorp (make-vector #(1 2 3) 10)))
  (let ((u #(1 2 3 1 2 3))
        (v #(3 2 1 3 2 1))
        (c (make-vector #(1 2) 6)))
    (format t "~&Testing of LINEAR-VECTORS starts with u = ~A and v = ~A~%"
            u v)
    (is (= (vector-dim u) 6))
    (is (= (vector-dim c) 6))
    (is (num= (e+ u v) #(4 4 4 4 4 4)))
    (is (and (num= (e+! c u v) #(4 4 4 4 4 4))
             (num= c #(4 4 4 4 4 4))))
    (is (num= (e- u v) #(-2 0 2 -2 0 2)))
    (is (num= (e-! c u v) #(-2 0 2 -2 0 2)))
    (is (and (num= (progn
                     (duplicate-vector #(1 2 3 1 2 3) c)
                     (e=-! c v))
                   #(-2 0 2 -2 0 2))
             (num= c #(-2 0 2 -2 0 2))))
    (is (and (num= (e=*! (e+! c u v) 2) #(8 8 8 8 8 8)) (num= c #(8 8 8 8 8 8)))))
  (is (num= (m* #(1 2 3) #(1 1 1)) 6))
  (is (num= (m* #(1 2 3) #(1 1 1)) (dot #(1 2 3) #(1 1 1))))
  (let ((u #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0))
        (v (make-array 6
                       :element-type 'double-float
                       :initial-contents (mapcar #'sin '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0)))))
    (is (num= (esin u) v))
    (is (num= (esin! u) v))))

(test linear-arrays
  (is (num= (zero-vector #2a((1 2 3) (2 4 5))) #2a((0 0 0) (0 0 0))))
  (is (num= (m* #2a((1 2 3) (4 5 6) (7 8 9)) #(1 2 3)) #(14 32 50)))
  (is (num= (m* #2a((1 2) (3 4)) #2a((3 4) (1 2))) #2a((5 8) (13 20))))
  (let ((A (make-array (list 5 5) :element-type 'double-float))
        (b (make-array 5 :element-type 'double-float))
        (num-gen (lambda ()  (coerce (funcall (gen-integer :min -10 :max 10))
                                'double-float))))
    (loop for i below 5
       do (progn
            (setf (aref b i) (funcall num-gen))
            (loop for j below 5
               do (setf (aref A i j) (funcall num-gen)))))
    (if (not (num= (lla:det A) 0.0d0))
        (let ((x (m/ A b)))
          (format t "~&Solving Ax=b~%")
          (is (num= (m* A x) b)))
        (progn
          (format t "det(A) = 0!~%")
          (is (= 1 1))))))

(test linear-mvectors
  (let ((u (up   0 1 2 3 4))
        (v (down 1 1 1 1 1)))
    (is (num= (mvref u 3) 3.0d0))
    (is (num= (e+ u (transpose v)) (up 1 2 3 4 5)))
    (is (num= (e- u (transpose v) 1.0d0) (up -2 -1 0 1 2)))))

(test linear-bicg
  (let ((A (make-array (list 5 5) :element-type 'double-float))
        (b (up 0 0 0 0 0))
        (x0 (up 1 1 1 1 1))
        (x (up 1 1 1 1 1))
        (num-gen (lambda ()  (coerce (funcall (gen-integer :min -10 :max 10))
                                'double-float))))
    (loop for i below 5
       do (progn
            (setf (mvref b i) (funcall num-gen))
            (setf (mvref x0 i) (funcall num-gen))
            (loop for j below 5
               do (setf (aref A i j) (funcall num-gen)))))
    (if (not (num= (lla:det A) 0.0d0))
        (progn
          (format t "~&Solving Ax=b~%")
          (format t "A = ~A~%" A)
          (format t "b = ~A~%" b)
          (let ((x (m/ A b x0 x)))
            (format t "x = ~A~%" x)
            (is (num= (m* A x) b))))
        (progn
          (format t "det(A) = 0!~%")
          (is (= 1 1))))))


(test linear-sparse-bicg
  (let ((A #2A((5 1 0 0 0) (-2 5 1 0 0) (0 -2 5 1 0) (0 0 -2 5 1) (0 0 0 -2 5)))
        (b (up 7 11 15 19 17))
        (x0 (up 1 20 10 10 50))
        (x (zero-mvector :up 5)))
    (let ((x (m/ A b x0 x)))
      (is (num= (m* A x) b)))))

;; (run! 'linear-sparse-bicg)


(defun run-linear-suite ()
  (run! 'linear-suite))

(run! 'linear-suite)
