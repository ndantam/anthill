;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2013, Georgia Tech Research Corporation
;;;; All rights reserved.
;;;;
;;;; Author(s): Neil T. Dantam <ntd@gatech.edu>
;;;; Georgia Tech Humanoid Robotics Lab
;;;; Under Direction of Prof. Mike Stilman
;;;;
;;;; This file is provided under the following "BSD-style" License:
;;;;
;;;;   Redistribution and use in source and binary forms, with or
;;;;   without modification, are permitted provided that the following
;;;;   conditions are met:
;;;;   * Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;;   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;;   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;;   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;;;   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;;;   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :motion-grammar)

(defstruct thread-pool
  threads         ; list of threads
  queue           ; work queue
  mutex           ; mutex
  cond            ; condition variable for workers
  finish-cond     ; to notify when finished
  counter         ; completed jobs
  counter-target  ; when to notify parent
  exit)

(defvar *thread-pool*)

(defparameter *in-thread* nil)

#+sbcl
(defun destroy-thread-pool (&optional (thread-pool *thread-pool*))
  (sb-thread:with-mutex ((thread-pool-mutex thread-pool))
    (setf (thread-pool-exit thread-pool) t))
  (sb-thread:condition-broadcast  (thread-pool-cond thread-pool)))


#+sbcl
(defun abort-thread-pool (&optional (thread-pool *thread-pool*))
  (loop for thread in (thread-pool-threads thread-pool)
     do (sb-thread:terminate-thread thread)))


#+sbcl
(defun init-thread-pool (&key (count 2))
  (when (and (boundp '*thread-pool*)
             *thread-pool*)
    (destroy-thread-pool *thread-pool*))
  (let ((tp (make-thread-pool
             :mutex (sb-thread:make-mutex)
             ;:mutex (sb-thread:make-mutex)
             :counter 0
             :cond (sb-thread:make-waitqueue)
             :finish-cond (sb-thread:make-waitqueue))))
    (labels ((run ()
               (let ((*in-thread* t))
                 (loop
                    for thunk =
                      (sb-thread:with-mutex ((thread-pool-mutex tp))
                        (loop until (or (thread-pool-exit tp)
                                        (thread-pool-queue tp))
                           do (sb-thread:condition-wait (thread-pool-cond tp)
                                                        (thread-pool-mutex tp))
                           finally (return (unless (thread-pool-exit tp)
                                             (pop (thread-pool-queue tp))))))
                    while thunk
                    do (progn
                         ;; do the work
                         (funcall thunk)
                         ;; increment work counter
                         (sb-thread:with-mutex ((thread-pool-mutex tp))
                           (incf (thread-pool-counter tp))
                           (when (= (thread-pool-counter tp)
                                    (thread-pool-counter-target tp))
                             (sb-thread:condition-broadcast (thread-pool-finish-cond tp)))))))))
      (setf (thread-pool-threads tp)
            (loop for i below count
               collect (sb-thread:make-thread #'run))))
    (setq *thread-pool* tp)))


#+sbcl
(defun thread-pool-push (thunk &optional (thread-pool *thread-pool*))
  (sb-thread:with-mutex ((thread-pool-mutex thread-pool))
    (etypecase thunk
      (list
       (setf (thread-pool-queue thread-pool)
             (append thunk (thread-pool-queue thread-pool))))
      (function  (push thunk (thread-pool-queue thread-pool) ))))
  (etypecase thunk
    (list (sb-thread:condition-broadcast (thread-pool-cond thread-pool)))
    (function (sb-thread:condition-notify (thread-pool-cond thread-pool)))))

;; TODO: have the parent thread process too
;;       Then, recursive calls won't stall
#+sbcl
(defun thread-pool-process (thunks &optional (thread-pool *thread-pool*))
  "Call every function in THUNKS, returning when all have finished."
  (assert (null (thread-pool-queue thread-pool)))
  ;; set pool vars
  (setf (thread-pool-counter-target thread-pool) (length thunks)
        (thread-pool-counter thread-pool) 0
        (thread-pool-queue thread-pool) thunks)
  ;; start workers
  (sb-thread:condition-broadcast (thread-pool-cond thread-pool))
  ;; wait for finish
  (sb-thread:with-mutex ((thread-pool-mutex thread-pool))
    (loop until (= (thread-pool-counter thread-pool)
                   (thread-pool-counter-target thread-pool))
       do (sb-thread:condition-wait (thread-pool-finish-cond thread-pool)
                                    (thread-pool-mutex thread-pool)))))

;; (defun thread-pool-map (result-type function sequence &key (parallel t))
;;   (loop for x in sequence
;;      collect (if parallel
;;                  (lambda () (

#+sbcl
(defun process-work-queue (generator &key (thread-count 8))
  (let* ((mutex (sb-thread:make-mutex))
         (thread-function (lambda ()
                            (loop for work-unit = (sb-thread:with-mutex (mutex)
                                                    (funcall generator))
                               while work-unit
                               do (funcall work-unit))))
         (threads (loop for i below thread-count
                     collect (sb-thread:make-thread thread-function))))
    (map nil #'sb-thread:join-thread threads)))


(defmacro do-parallel ((var set &key result (parallel '(not *in-thread*))) &body body)
  "Process elements from collection in parallel"
  (alexandria:with-gensyms (set-var fun)
    (let ((parallel-case
           `(thread-pool-process (map-finite-set 'list (lambda (,var) (thunk (,fun ,var))) ,set-var)))
          (serial-case
           `(etypecase ,set-var
              (tree-set (map-tree-set nil #',fun ,set))
              (list (dolist (,var ,set-var ,result)
                      (,fun ,var)))
              (array
               (loop for ,var across ,set
                  do (,fun ,var)))
              (hash-table
               (loop for ,var being the hash-keys of ,set
                  do (,fun ,var))))))
    `(let ((,set-var ,set))
       (labels ((,fun (,var) ,@body))
         ,(cond
           (t
            `(if (and ,parallel *thread-pool*)
                 ,parallel-case
                 ,serial-case))))
       ,result))))
