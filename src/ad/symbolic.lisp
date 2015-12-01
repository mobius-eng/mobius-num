(in-package mobius.numeric.symbolic)
;; * Symbolic extension
;; ** Unary numerical operations
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun make-unary-symbolic (tag num-fun)
   (lambda (x)
     (cond ((cl:numberp x) (funcall num-fun x))
           (:else (list tag x))))))

(defmacro defsymb (fun tag op)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-function ',fun)
           (make-unary-symbolic ',tag #',op))))

(defsymb sin sin cl:sin)
(defsymb cos cos cl:cos)
(defsymb tan tan cl:tan)
(defsymb exp exp cl:exp)
(defsymb sqrt sqrt cl:sqrt)
(defsymb asin asin cl:asin)
(defsymb acos acos cl:acos)
(defsymb atan atan cl:atan)
(defsymb sinh sinh cl:sinh)
(defsymb cosh cosh cl:cosh)
(defsymb tanh tanh cl:tanh)

;; (defsymb log log cl:log)

;; ** Binary numerical operations

(defun log (arg &optional base)
  (cond ((cl:numberp arg)
         (cond ((null base) (cl:log arg))
               ((cl:numberp base) (cl:log arg base))
               (t `(log ,arg ,base))))
        (t
         (unless base
           (setf base (cl:exp 1.0d0)))
         `(log ,arg ,base))))

(defun expt (base power)
  (cond ((and (cl:numberp base) (cl:numberp power))
         (cl:expt base power))
        (t `(expt ,base ,power))))



;; ** Multiarity numerical operations

(defgeneric argument-type (x))
(defmethod argument-type (x) :else)
(defmethod argument-type ((x number)) :number)
(defmethod argument-type ((x symbol)) :symbol)
(defmethod argument-type ((x function)) :function)
(defmethod argument-type ((x cons)) :cons)

;; (defun argument-type (x)
;;   (cond ((cl:numberp x) :number)
;;         ((cl:symbolp x) :symbol)
;;         ((cl:functionp x) :function)
;;         (t :else)))

(defun add-functions (&rest more)
  (match more
    (nil nil)
    ((list x) x)
    ((list x y) (lambda (z) (+ (funcall x z) (funcall y z))))
    ((list* f more)
     (lambda (z) (apply #'+ (funcall f z) (mapcar (lambda (g) (funcall g z)) more))))))

(defun mul-functions (&rest more)
  (match more
    (nil nil)
    ((list x) x)
    ((list x y) (lambda (z) (* (funcall x z) (funcall y z))))
    ((list* f more)
     (lambda (z) (apply #'* (funcall f z) (mapcar (lambda (g) (funcall g z)) more))))))



(defun three-way-split (pred list)
  (loop for x in list
     for p = (funcall pred x) then (funcall pred x)
     if (eq p :left)
     collect x into left
     else if (eq p :middle)
     collect x into middle
     else
     collect x into right
     finally (return (list left middle right))))

(defun separate-nums-funs (x)
  (cond ((cl:numberp x) :left)
        ((cl:functionp x) :middle)
        (t :right)))


(defun two-way-split (predicate list)
  (loop for x in list
     if (funcall predicate x)
     collect x into truths
     else
     collect x into falses
     end
     finally (return (list truths falses))))

(defun bin+ (&rest more)
  (flet ((expr-from-arg-list (list)
           (destructuring-bind (nums others) (two-way-split #'cl:numberp list)
             (let ((num (apply #'+ nums)))
               (cond ((null others) num)
                     ((cl:zerop num) (if (null (rest others)) (first others) `(+ ,@others)))
                     (t `(+ ,num ,@others)))))))
    (match more
      (nil 0)
      ((list x) x)
      ((list x y)
       (match (list (argument-type x) x (argument-type y) y)
         ((list :number x :number y) (cl:+ x y))
         ((list :number 0 _ y) y)
         ((list _ x :number 0) x)
         ((list :function f :function g)
          (lambda (z) (bin+ (funcall f z) (funcall g z))))
         ((list :function f _ y)
          (lambda (z) (bin+ (funcall f z) y)))
         ((list _ x :function g)
          (lambda (z) (bin+ x (funcall g z))))
         ((list :cons (list* '+ p) :cons (list* '+ u))
          (expr-from-arg-list (append p u)))
         ((list :cons (list* '+ p) _ y)
          (expr-from-arg-list (cons y p)))
         ((list _ x :cons (list* '+ u))
          (expr-from-arg-list (cons x u)))
         (otherwise `(+ ,x ,y)))))))


(defun bin* (&rest more)
  (flet ((expr-from-arg-list (list)
           (destructuring-bind (nums others) (two-way-split #'cl:numberp list)
             (let ((num (apply #'* nums)))
               (cond ((null others) num)
                     ((cl:zerop num) 0)
                     ((cl:= num 1) (if (null (rest others)) (first others) `(* ,@others)))
                     (t `(* ,num ,@others)))))))
    (match more
      (() 1)
      ((list x) x)
      ((list x y)
       (match (list (argument-type x) x (argument-type y) y)
         ((list :number x :number y) (cl:* x y))
         ((list :number 0 _ _) 0)
         ((list _ _ :number 0) 0)
         ((list :number 1 _ y) y)
         ((list _ x :number 1) x)
         ((list :function f :function g)
          (lambda (z) (bin* (funcall f z) (funcall g z))))
         ((list :function f _ y)
          (lambda (z) (bin* (funcall f z) y)))
         ((list _ x :function g)
          (lambda (z) (bin* x (funcall g z))))
         ((list :cons (list* '* p) :cons (list* '* u))
          (expr-from-arg-list (append p u)))
         ((list :cons (list* '* p) _ y)
          (expr-from-arg-list (cons y p)))
         ((list _ x :cons (list* '* u))
          (expr-from-arg-list (cons x u)))
         (otherwise `(* ,x ,y)))))))


(defun bin- (&rest more)
  (flet ((expr-from-arg-list (lead list)
           (destructuring-bind (nums others) (two-way-split #'cl:numberp list)
             (let ((num (apply #'+ nums)) front)
               (cond ((cl:numberp lead) (setf num (cl:- lead num)) (setf front :number))
                     (t (setf front lead)))
               (cond ((null others)
                      (case front
                        ((:number) num)
                        (t (if (cl:zerop num) front `(- ,front ,num)))))
                     ((cl:zerop num)
                      (case front
                        ((:number) (if (null (rest others))
                                       `(- ,(first others))
                                       `(- 0 ,@others)))
                        (t `(- ,front ,@others))))
                     (t (case front
                          ((:number) `(- ,num ,@others))
                          (t `(- ,front ,num ,@others)))))))))
    (match more
      ((list x)
       (match (list (argument-type x) x)
         ((list :number x) (cl:- x))
         ((list :function f) (lambda (z) (- (funcall f z))))
         (otherwise `(- ,x))))
      ((list x y)
       (match (list (argument-type x) x (argument-type y) y)
         ((list :number x :number y) (cl:- x y))
         ((list :number 0 _ y) (bin- y))
         ((list _ x :number 0) x)
         ((list :function f :function g)
          (lambda (z) (bin- (funcall f z) (funcall g z))))
         ((list :function f _ y)
          (lambda (z) (bin- (funcall f z) y)))
         ((list _ x :function g)
          (lambda (z) (bin- x (funcall g z))))
         ((list :cons (list* '+ _) :cons (list* '+ _))
          (expr-from-arg-list x (cdr y)))
         ((list :cons (list* '- p) :cons (list* '+ u))
          (expr-from-arg-list (first p) (append (rest p) u)))
         ((list :cons (list* '- p) _ y)
          (expr-from-arg-list (first p) (append (rest p) (list y))))
         (otherwise `(- ,x ,y)))))))


(defun bin/ (&rest more)
  (flet ((expr-from-arg-list (lead list)
           (destructuring-bind (nums others) (two-way-split #'cl:numberp list)
             (let ((num (apply #'* nums)) front)
               (cond ((cl:numberp lead) (setf num (cl:/ lead num)) (setf front :number))
                     (t (setf front lead)))
               (cond ((null others)
                      (case front
                        ((:number) num)
                        (t (if (cl:= num 1) front `(/ ,front ,num)))))
                     ((and (cl:zerop num) (eq front :number)) 0)
                     ((and (cl:= num 1) (not (eq front :number)))
                      `(/ ,front ,@others))
                     (t (case front
                          ((:number) `(/ ,num ,@others))
                          (t `(/ ,front ,num ,@others)))))))))
    (match more
      ((list x)
       (match (list (argument-type x) x)
         ((list :number x) (cl:/ x))
         ((list :function f) (lambda (z) (/ (funcall f z))))
         (otherwise `(/ ,x))))
      ((list x y)
       (match (list (argument-type x) x (argument-type y) y)
         ((list :number x :number y) (cl:/ x y))
         ((list :number 0 _ _) 0)
         ((list _ x :number 1) x)
         ((list :function f :function g)
          (lambda (z) (bin/ (funcall f z) (funcall g z))))
         ((list :function f _ y)
          (lambda (z) (bin/ (funcall f z) y)))
         ((list _ x :function g)
          (lambda (z) (bin/ x (funcall g z))))
         ((list :cons (list* '* _) :cons (list* '* _))
          (expr-from-arg-list x (cdr y)))
         ((list :cons (list* '/ p) :cons (list* '* u))
          (expr-from-arg-list (first p) (append (rest p) u)))
         ((list :cons (list* '/ p) _ y)
          (expr-from-arg-list (first p) (append (rest p) (list y))))
         (otherwise `(/ ,x ,y)))))))

(defun + (&rest more)
  (flet ((bin+ (x y)
           (match (list (argument-type x) x (argument-type y) y)
             ((list :number x :number y) (cl:+ x y))
             ((list :number 0 _ y) y)
             ((list _ x :number 0) x)
             ((list :function x :function y) (add-functions x y))
             ((list :function x _ y) (lambda (z) (+ (funcall x z) y)))
             ((list _ x :function y) (lambda (z) (+ x (funcall y z))))
             (otherwise `(+ ,x ,y)))))
    (match more
      (nil 0)
      ((list x) x)
      ((list x y) (bin+ x y))
      (otherwise
       (destructuring-bind (nums funs others)
           (three-way-split #'separate-nums-funs more)
         (let ((num (apply #'cl:+ nums))
               (fun (apply #'add-functions funs)))
           (cond ((not (null funs))
                  (lambda (z) (apply #'+ num (funcall fun z) others)))
                 ((null others) num)
                 ((cl:zerop num) (if (rest others)
                                     `(+ ,@others)
                                     (first others)))
                 (t `(+ ,num ,@others)))))))))

(defun - (x &rest more)
  (match more
    (nil (cond ((cl:numberp x) (cl:- x))
               ((cl:functionp x) (lambda (z) (- (funcall x z))))
               (t `(- ,x))))
    ((list y)
     (match (list (argument-type x) x (argument-type y) y)
       ((list :number x :number y) (cl:- x y))
       ((list :number 0 _ y) (- y))
       ((list _ x :number 0) x)
       ((list :function f :function g) (lambda (z) (- (funcall f z)
                                                      (funcall g z))))
       ((list :function f _ y) (lambda (z) (- (funcall f z) y)))
       ((list _ x :function g) (lambda (z) (- x (funcall g z))))
       (otherwise `(- ,x ,y))))
    (otherwise
     (destructuring-bind (nums funs others)
         (three-way-split #'separate-nums-funs more)
       (let ((num (apply #'cl:+ nums))
             (fun (apply #'add-functions funs))
             (front nil))
         (cond ((cl:numberp x) (setf num (cl:- x num)) (setf front :num))
               ((cl:functionp x)
                (setf fun (if fun
                              (lambda (z) (- (funcall x z) (funcall fun z)))
                              x))
                (setf front :fun))
               (t (setf front x)))
         (cond ((not (null fun))
                (case front
                  ((:num) (lambda (z) (apply #'- num (funcall fun z) more)))
                  ((:fun) (lambda (z) (apply #'- (funcall fun z) num more)))
                  (t (lambda (z) (apply #'- front num (funcall fun z) more)))))
               ((null others)
                (case front
                  ((:num) num)
                  (t (if (cl:zerop num) front `(- ,front ,num)))))
               ((cl:zerop num)
                (case front
                  ((:num) `(- 0 ,@others))
                  (t `(- ,front ,@others))))
               (t (case front
                    ((:num) `(- ,num ,@others))
                    (t `(- ,front ,num ,@others))))))))))

(defun * (&rest more)
  (flet ((bin* (x y)
           (match (list (argument-type x) x (argument-type y) y)
             ((list :number x :number y) (cl:* x y))
             ((list :number 0 _ _) 0)
             ((list _ _ :number 0) 0)
             ((list :number 1 _ y) y)
             ((list _ x :number 1) x)
             ((list :function x :function y) (mul-functions x y))
             ((list :function x _ y) (lambda (z) (* (funcall x z) y)))
             ((list _ x :function y) (lambda (z) (* x (funcall y z))))
             (otherwise `(* ,x ,y)))))
    (match more
      (nil 1)
      ((list x) x)
      ((list x y) (bin* x y))
      (otherwise
       (destructuring-bind (nums funs others)
           (three-way-split #'separate-nums-funs more)
         (let ((num (apply #'cl:* nums))
               (fun (apply #'mul-functions funs)))
           (cond ((cl:zerop num) 0)
                 ((not (null funs))
                  (lambda (z) (apply #'* num (funcall fun z) others)))

                 ((null others) num)
                 ((cl:= num 1) (if (rest others) `(* ,@others) (first others)))
                 (t `(* ,num ,@others)))))))))

(defun / (x &rest more)
  (match more
    (nil (cond ((cl:numberp x) (cl:/ x))
               ((cl:functionp x) (lambda (z) (/ (funcall x z))))
               (t `(/ ,x))))
    ((list y)
     (match (list (argument-type x) x (argument-type y) y)
       ((list :number x :number y) (cl:/ x y))
       ((list :number 1 _ y) (/ y))
       ((list _ x :number 1) x)
       ((list :function f :function g) (lambda (z) (/ (funcall f z)
                                                      (funcall g z))))
       ((list :function f _ y) (lambda (z) (/ (funcall f z) y)))
       ((list _ x :function g) (lambda (z) (/ x (funcall g z))))
       (otherwise `(/ ,x ,y))))
    (otherwise
     (destructuring-bind (nums funs others)
         (three-way-split #'separate-nums-funs more)
       (let ((num (apply #'cl:* nums))
             (fun (apply #'mul-functions funs))
             (front nil))
         (cond ((cl:numberp x) (setf num (cl:/ x num)) (setf front :num))
               ((cl:functionp x)
                (setf fun (if fun
                              (lambda (z) (/ (funcall x z) (funcall fun z)))
                              x))
                (setf front :fun))
               (t (setf front x)))
         (cond ((and (eq front :num) (cl:zerop num)) 0)
               ((not (null fun))
                (case front
                  ((:num) (lambda (z) (apply #'/ num (funcall fun z) more)))
                  ((:fun) (lambda (z) (apply #'/ (funcall fun z) num more)))
                  (t (lambda (z) (apply #'/ front num (funcall fun z) more)))))
               ((null others)
                (case front
                  ((:num) num)
                  (t (if (cl:= num 1) front `(/ ,front ,num)))))
               ((cl:= num 1)
                (case front
                  ((:num) `(/ 1 ,@others))
                  (t `(/ ,front ,@others))))
               (t (case front
                    ((:num) `(/ ,num ,@others))
                    (t `(/ ,front ,num ,@others))))))))))


;; ** Comparison

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun make-symbolic-comparison (bin-comp)
   (lambda (x &rest more)
     (match more
       (nil t)
       ((list y) (funcall bin-comp x y))
       (otherwise (loop for y on (cons x more)
                     when (and (second y) (not (funcall bin-comp (first y) (second y))))
                     return nil
                     end
                     finally (return t)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function '=)
        (make-symbolic-comparison (lambda (x y)
                                    (match (list (argument-type x) x (argument-type y) y)
                                      ((list :number x :number y) (cl:= x y))
                                      ((list :symbol x :symbol y) (eq x y))
                                      (otherwise nil)))))

  (setf (symbol-function '<)
        (make-symbolic-comparison (lambda (x y)
                                    (match (list (argument-type x) x (argument-type y) y)
                                      ((list :number x :number y) (cl:< x y))
                                      (otherwise nil)))))

  (setf (symbol-function '>)
        (make-symbolic-comparison (lambda (x y)
                                    (match (list (argument-type x) x (argument-type y) y)
                                      ((list :number x :number y) (cL:> x y))
                                      (otherwise nil)))))

  (setf (symbol-function '<=)
        (make-symbolic-comparison (lambda (x y)
                                    (match (list (argument-type x) x (argument-type y) y)
                                      ((list :number x :number y) (cl:<= x y))
                                      ((list :symbol x :symbol y) (eq x y))
                                      (otherwise nil)))))


  (setf (symbol-function '>=)
        (make-symbolic-comparison (lambda (x y)
                                    (match (list (argument-type x) x (argument-type y) y)
                                      ((list :number x :number y) (cl:>= x y))
                                      ((list :symbol x :symbol y) (eq x y))
                                      (otherwise nil))))))

(defun zerop (x)
  (and (cl:numberp x) (cl:zerop x)))

(defun plusp (x)
  (and (cl:numberp x) (cl:plusp x)))

(defun minusp (x)
  (and (cl:numberp x) (cl:minusp x)))