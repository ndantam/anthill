(in-package :anthill)

(declaim (ftype (function ((or (eql list)
                               (eql vector)
                               null)
                           (function (t) t)
                           sequence)
                          sequence)
                pmap))
(defun pmap (result-type function sequence)
  (let* ((n (length sequence))
         (result (when result-type (make-array n :initial-element nil)))
         (f (if result
                (lambda (x i)
                  (setf (aref result i) (funcall function x)))
                (lambda (x i)
                  (declare (ignore i))
                  (funcall function x)))))
    ;; Iterate
    (etypecase sequence
      (list (pdo ((list sequence (cdr list))
                  (i 0 (1+ i)))
                ((null list))
              (funcall f (car list) i)))
      (vector (pdo ((i 0 (1+ i)))
                  ((>= i n))
                (funcall f (aref sequence i) i))))
    ;; Result
    (if (eq 'list result-type)
        (loop for i across result
           collect i)
        result)))



;; (defun pmap (result-type function sequence)
;;   (let* ((n (length sequence))
;;          (result (when result-type (make-array n :initial-element nil)))
;;          (i 0)
;;          (next (etypecase sequence
;;                  (list (let ((list sequence))
;;                          (lambda ()
;;                            (prog1 (car list)
;;                              (setq list (cdr list))))))
;;                  (vector (lambda () (aref sequence i))))))
;;     (pgen (lambda ()
;;             (when (< i n)
;;               (let ((j i)
;;                     (x (funcall next)))
;;                 (incf i)
;;                 (lambda ()
;;                   (let ((y  (funcall function x)))
;;                     (when result
;;                       (setf (aref result j) y))))))))
;;     (if (eq 'list result-type)
;;         (loop for i across result
;;            collect i)
;;         result)))

;; (defmacro pdolist ((var list &optional result) &body  body)
;;   (with-gensyms (slist)
;;     `(let ((,slist ,list))
;;        (pgen (lambda ()
;;                (when ,slist
;;                  (let ((,var (car ,slist)))
;;                    (setq ,slist (cdr ,slist))
;;                    (lambda () ,@body)))))
;;        ,result)))
