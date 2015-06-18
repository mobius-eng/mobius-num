(in-package mobius.numeric.criteria)

(defstruct criteria list)

(defun add-to-list! (criteria id new-criterium)
  (push (cons id new-criterium) criteria))

(defun remove-from-list! (criteria id)
  (delete id (criteria-list criteria) :key #'car))

(defun init-criterium (criterium) (funcall (cdr criterium)))

(defun build (criteria)
  (let ((all-criteria (mapcar #'init-criterium
                              (criteria-list criteria))))
    #'(lambda (x)
        (let ((y (iterator:continue x)))
          (loop for c in all-criteria
             if (iterator:continue? y)
             do (setf y (funcall c y))
             else
             return y
             end
             finally (return y))))))

(defun make (&rest criteria)
  (make-criteria :list criteria))

(defmacro define-criterium (name (args x &rest init) &body body)
  `(defun ,name ,args
     (cons ',name
           (lambda ()
             ,(if init
                  `(let ,init
                     (lambda (,x)
                       ,@body
                       ,x))
                  `(lambda (,x)
                     ,@body
                     ,x))))))

(define-criterium finished-value ((predicate &optional info) x)
  (when (funcall predicate (iterator:value x))
    (iterator:add-info! (iterator:to-finished! x) 'finished-value info)))

(define-criterium failed-value ((predicate &optional info) x)
  (when (funcall predicate (iterator:value x))
    (iterator:add-info! (iterator:to-failed! x) 'failed-value info)))

(define-criterium log-value ((msg-fun) x)
  (funcall msg-fun (iterator:value x)))


(define-criterium converged ((close? &optional info) x (last-x nil))
  (let ((v (iterator:value x)))
    (cond ((null last-x) (setf last-x v))
          ((funcall close? last-x v)
           (iterator:add-info! (iterator:to-finished! x) 'converged info))
          (t (setf last-x v)))))

(define-criterium limit-iterations ((max-count) x (left max-count))
  (decf left)
  (when (<= left 0)
    (iterator:add-info! (iterator:to-failed! x)
                        'limit-iterations
                        (format nil "Max count reached ~D" max-count))))

(define-criterium modify-value ((modifier) x)
  (let ((v (iterator:value x)))
    (setf (iterator:value x) (funcall modifier v))))

