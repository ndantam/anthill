;;;; Copyright (c) 2015, Rice University
;;;; Copyright (c) 2020, Colorado School of Mines
;;;;
;;;; All rights reserved.
;;;;
;;;; Author(s): Neil T. Dantam <ntd@gatech.edu>
;;;;
;;;;   Redistribution and use in source and binary forms, with or
;;;;   without modification, are permitted provided that the following
;;;;   conditions are met:
;;;;
;;;;   * Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;   * Neither the name of copyright holder the names of its
;;;;     contributors may be used to endorse or promote products
;;;;     derived from this software without specific prior written
;;;;     permission.
;;;;
;;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;;   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;;   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;;   AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;;   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;;;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;;   POSSIBILITY OF SUCH DAMAGE.

(in-package :anthill)

(defun %pdo (let-symbol setq-symbol varlist end-test-form result-form body)
  (let ((vars) (init-forms) (step-forms)
        (declarations (loop for b in body
                         while (eq (car b) 'declare)
                         collect b))
        (real-body (loop for bb on body
                      while (eq (caar bb) 'declare)
                      finally (return bb))))
    ;; Parse varlist
    (dolist (v (reverse varlist))
      (destructuring-bind  (var &optional init-form step-form) v
        (push var vars)
        (push init-form init-forms)
        (push step-form step-forms)))

    ;; Result
    `(,let-symbol (,@(map 'list #'list vars init-forms))
       (pgen (lambda ()
               (unless ,end-test-form
                 (prog1 (let (,@(map 'list #'list vars vars))
                          ,@declarations
                          (lambda () ,@real-body))
                   (,setq-symbol ,@(loop
                                     for var in vars
                                     for step-form in step-forms
                                     when step-form
                                     nconc `(,var ,step-form)))))))
       ,(when result-form
          `(values ,@result-form)))))

(defmacro pdo (varlist
               (end-test-form &rest result-form)
               &body body)
  (%pdo 'let 'psetq
        varlist end-test-form result-form body))

(defmacro pdo* (varlist
               (end-test-form &rest result-form)
               &body body)
  (%pdo 'let* 'setq
        varlist end-test-form result-form body))

(defmacro pdolist ((var list &optional result) &body  body)
  (with-gensyms (current-list)
    `(pdo* ((,current-list ,list (cdr ,current-list))
            (,var (car ,current-list) (car ,current-list)))
         ((null ,current-list) ,result)
       (declare (ignore ,current-list))
       ,@body)))

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
