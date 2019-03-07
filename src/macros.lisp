(in-package #:noloop.assert-p)

(defmacro with-gensyms (vars &body body)
  `(let ,(loop for v in vars collect `(,v (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(with-gensyms (,@gensyms)
       `(let (,,@(loop for g in gensyms
                       for n in names
                       collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names
                         for g in gensyms
                         collect `(,n ,g)))
             ,@body)))))

(defmacro assertion-error-check (test-form)
  (with-gensyms (result)
    `(let ((,result t))
       (handler-case ,test-form
         (assertion-error (c)
           (declare (ignore c))
           (setf ,result nil)))
       ,result)))

(defmacro assertion-values (test-fn predicate actual expected operator)
  (once-only (test-fn predicate actual expected)
    `(assertion (funcall (symbol-function ,test-fn)
                         ,predicate
                         ,actual)
                ,actual
                ,expected
                ,operator)))

(defmacro error-check (test-fn)
  (with-gensyms (result)
    `(let ((,result nil))
       (handler-case (funcall ,test-fn)
         (error (c)
           (declare (ignore c))
           (setf ,result t)))
       ,result)))

(defmacro condition-error-check (test-fn condition)
  (with-gensyms (result)
    `(let ((,result nil))
       (handler-case (funcall ,test-fn)
         (,condition (c)
           (declare (ignore c))
           (setf ,result t))
         (error (c)
           (declare (ignore c))
           (setf ,result nil)))
       ,result)))

;; (defmacro condition-error-p (test-fn condition)
;;   `(assertion
;;     (let ((result nil))
;;       (handler-case (funcall ,test-fn)
;;         (,condition (c)
;;           (declare (ignore c))
;;           (setf result t))
;;         (error (c)
;;           (declare (ignore c))
;;           (setf result nil)))
;;       result)
;;     'test-fn
;;     'error
;;     'catch-error))

