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
         (i 0)
         (next (etypecase sequence
                 (list (let ((list sequence))
                         (lambda ()
                           (prog1 (car list)
                             (setq list (cdr list))))))
                 (vector (lambda () (aref sequence i))))))
    (pgen (lambda ()
            (when (< i n)
              (let ((j i)
                    (x (funcall next)))
                (incf i)
                (lambda ()
                  (let ((y  (funcall function x)))
                    (when result
                      (setf (aref result j) y))))))))
    (if (eq 'list result-type)
        (loop for i across result
           collect i)
        result)))

(defmacro pdolist ((var list &optional result) &body  body)
  (with-gensyms (slist)
    `(let ((,slist ,list))
       (pgen (lambda ()
               (when ,slist
                 (let ((,var (car ,slist)))
                   (setq ,slist (cdr ,slist))
                   (lambda () ,@body)))))
       ,result)))