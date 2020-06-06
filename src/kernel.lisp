;;;; Copyright (c) 2013, Georgia Tech Research Corporation
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

(defparameter *thread-count* 8)


;;; Mutable Queue

(defstruct mqueue
  list
  lastcons)

(defun mqueue ()
  (make-mqueue))

(defun %mqueue-enqueue (mq list last)
  (if (mqueue-lastcons mq)
      (progn
        (assert (mqueue-list mq))
        (rplacd (mqueue-lastcons mq) list))
      (progn
        (assert (not (mqueue-list mq)))
        (setf (mqueue-list mq) list)))
  (setf (mqueue-lastcons mq) last)
  mq)

(defun mqueue-enqueue (mq item)
  (let ((c (cons item nil)))
    (%mqueue-enqueue mq c c)))



(defun mqueue-emptyp (mq)
  (if (mqueue-list mq)
      t
      nil))

(defun mqueue-dequeue (mq)
  (if-let ((list (mqueue-list mq)))
    ;; Non-Empty
    (progn
      (assert (mqueue-lastcons mq))
      ;; Remove
      (let ((cdr (cdr list)))
        (setf (mqueue-list mq) cdr)
        (unless cdr ; empty now
          (setf (mqueue-lastcons mq)  nil)))
      ;; Result
      (values (car list) t))
    ;; Empty
    (values nil nil)))


;;; TODO: Thread Pool Implementation

;; (defstruct work-unit
;;   (thunk #'identity :type function)
;;   (error-handler nil :type (or null function))
;;   (finalizer nil :type (or null function)))


(defstruct thread-pool
  (mutex (sb-thread:make-mutex))
  (waitqueue (sb-thread:make-waitqueue))
  (threads nil :type list)
  (queue (make-mqueue) :type mqueue)
  (exit nil :type boolean))


(defvar *thread-pool* nil)

(defmacro with-thread-pool-lock ((thread-pool) &body body)
  `(sb-thread:with-mutex ((thread-pool-mutex ,thread-pool))
     ,@body))


(defmacro wait-thread-pool ((thread-pool) &body test-forms)
  (with-gensyms (wq mutex tp x)
    `(loop
        with ,tp = ,thread-pool
        with ,wq = (thread-pool-waitqueue ,tp)
        with ,mutex = (thread-pool-mutex ,tp)
        for ,x = (progn ,@test-forms)
        until ,x
        do (sb-thread:condition-wait ,wq ,mutex)
        finally (return ,x))))

(defun thread-pool-function (thread-pool)
  (lambda ()
    (do ((unit)
         (exit))
        (exit)
      ;; Get unit
      (with-thread-pool-lock (thread-pool)
        (wait-thread-pool (thread-pool)
          (setq unit (mqueue-dequeue (thread-pool-queue thread-pool)))
          (cond (unit t)
                ((thread-pool-exit thread-pool)
                 (setq exit t)))))
      ;; Run unit
      (when unit
        (ignore-errors (funcall unit))))))


(defun thread-pool-enqueue (thunk
                            &key
                              error-handler
                              finalizer
                              (thread-pool *thread-pool*))
  (let ((unit (lambda ()
                (flet ((h ()
                         (if error-handler
                             (handler-case (funcall thunk)
                               (error (e) (funcall error-handler e)))
                             (funcall thunk))))
                (if finalizer
                    ;; Have finalizer
                    (unwind-protect (h) (funcall finalizer))
                    ;; No finalizer
                    (h))))))
    (with-thread-pool-lock (thread-pool)
      ;;(print  (thread-pool-queue thread-pool))
      (mqueue-enqueue (thread-pool-queue thread-pool) unit)
      (sb-thread:condition-notify (thread-pool-waitqueue thread-pool)))))

;; (list
;;  (let* ((units (loop for thunk in thunks
;;                   collect
;;                     (make-work-unit :thunk thunk
;;                                     :error-handler error-handler)))
;;         (last (last units)))
;;    (with-thread-pool-lock (thread-pool)
;;      (%mqueue-enqueue (thread-pool-queue thread-pool) units last)
;;      (sb-thread:condition-broadcast (thread-pool-waitqueue thread-pool)))))


(defun start-thread-pool (&key (thread-count *thread-count*))
  (when (and (boundp '*thread-pool*)
             *thread-pool*)
    (error "Thread pool is already running"))
  (let ((tp (make-thread-pool)))
    (setf (thread-pool-threads tp)
          (loop for i below thread-count
             collect (sb-thread:make-thread (thread-pool-function tp)
                                            :name (format nil "thread-pool-~D" i))))
      (setq *thread-pool* tp)))

(defun stop-thread-pool (&key (thread-pool *thread-pool*) (wait t))
  (when thread-pool
    (with-thread-pool-lock (thread-pool)
      (setf (thread-pool-exit thread-pool) t)
      (sb-thread:condition-broadcast  (thread-pool-waitqueue thread-pool)))
    (when wait
      (map nil #'sb-thread:join-thread (thread-pool-threads thread-pool)))
    (when (eq thread-pool *thread-pool*)
      (setq *thread-pool* nil))))



;; TODO: recursive thread pooling:
;; - this thread helps until its results are finished
;; -

(defun pgen (generator)
  (let ((thread-pool *thread-pool*)
        (mutex (sb-thread:make-mutex))
        (waitqueue (sb-thread:make-waitqueue))
        (err nil)
        (count 1)) ;; start count at one to avoid racing with finished
                   ;; work units

    ;; Enqueue
    (loop
       for thunk =
         (unless (sb-thread:with-mutex (mutex) err)
           (funcall generator))       ; TODO: what if we have
                                        ; an exception here and
                                        ; in a queue thunk?
       while thunk
       do
       ;; Increment Count
         (sb-thread:with-mutex (mutex)
           (incf count))
       ;; Enqueue Thunk
         (let ((thunk thunk))
           (thread-pool-enqueue thunk
                                :error-handler (lambda (e)
                                                 (sb-thread:with-mutex (mutex)
                                                   (setq err e)))
                                :finalizer (lambda ()
                                             (sb-thread:with-mutex (mutex)
                                               (decf count)
                                               (when (zerop count)
                                                 (sb-thread:condition-notify waitqueue))))
                                :thread-pool thread-pool)))

    ;; Wait
    (sb-thread:with-mutex (mutex)
      (decf count)
      (loop until (zerop count)
         do (sb-thread:condition-wait waitqueue mutex)))

    ;; End
    (when err (error err))
    (values)))


;; (defun pgen (generator)
;;   (let* ((mutex (sb-thread:make-mutex))
;;          (condition))
;;     (labels ((try-thunk (thunk)
;;                (unless condition
;;                  (handler-case (funcall thunk)
;;                    (error (c)
;;                      (setq condition c)
;;                      nil))))
;;              (thread-function ()
;;                (loop
;;                   for work-unit = (sb-thread:with-mutex (mutex)
;;                                     (try-thunk generator))
;;                   while work-unit
;;                   do (try-thunk work-unit))))
;;       (let ((threads (loop for i below *thread-count*
;;                         collect (sb-thread:make-thread #'thread-function))))
;;         (map nil #'sb-thread:join-thread threads)
;;         (when condition
;;           (error condition))))))



;; (defstruct sync-unit
;;   mutex
;;   cond)

;; (defstruct (%work-unit (:include sync-unit))
;;   thunk
;;   finished)

;; (defstruct (generator-work-unit (:include %work-unit)))
;; (defstruct (singleton-work-unit (:include %work-unit)))

;; (defstruct (thread-pool (:include sync-unit))
;;   threads         ; list of threads
;;   queue           ; work queue
;;   exit)


;; (defmacro with-sync-unit ((sync-unit) &body body)
;;   `(sb-thread:with-mutex ((sync-unit-mutex ,sync-unit))
;;      ,@body))

;; (defmacro wait-sync-unit ((sync-unit) &body test-forms)
;;   (with-gensyms (condition mutex su x)
;;     `(loop
;;         with ,su = ,sync-unit
;;         with ,condition = (sync-unit-cond ,su)
;;         with ,mutex = (sync-unit-mutex ,su)
;;         for ,x = (progn ,@test-forms)
;;         until ,x
;;         do (sb-thread:condition-wait ,condition ,mutex)
;;         finally (return ,x))))

;; (defun notify-sync-unit (sync-unit)
;;   (sb-thread:condition-broadcast (sync-unit-cond sync-unit)))

;; (defun thread-pool-function (thread-pool)
;;   (lambda ()
;;     (let ((quit nil))
;;       (labels ((h ()
;;                  (let ((work-unit
;;                         (with-sync-unit (thread-pool)
;;                           (wait-sync-unit (thread-pool)
;;                             (if (thread-pool-exit thread-pool)
;;                                 (setq quit t) ; exit
;;                                 ;; check for a work unit
;;                                 (when-let ((q (thread-pool-queue thread-pool)))
;;                                   (car q)))))))
;;                    (when (and (not quit)
;;                               work-unit)
;;                      (handle-gen-unit work-unit))))
;;                (handle-gen-unit (work-unit)
;;                  (when-let
;;                      ((thunk
;;                        (etypecase work-unit
;;                          ;; TODO: singleton units
;;                          ;; TODO: exceptions
;;                          (generator-work-unit
;;                           (with-sync-unit (work-unit)
;;                             (unless (generator-work-unit-finished work-unit)
;;                               (let ((thunk (funcall (generator-work-unit-thunk work-unit))))
;;                                 ;; check if finished
;;                                 (unless thunk
;;                                   ;; remove from work queue
;;                                   (with-sync-unit (thread-pool)
;;                                     (setf (thread-pool-queue thread-pool)
;;                                           (delete work-unit (thread-pool-queue thread-pool) :test #'eq)))
;;                                   ;; mark finished
;;                                   (setf (generator-work-unit-finished work-unit) t)
;;                                   ;; notify
;;                                   (notify-sync-unit work-unit))
;;                                 ;; result
;;                                 thunk)))))))
;;                    ;; Body
;;                    (funcall thunk))))

;;         (loop until quit
;;            do (h))))))


;; (defun start-thread-pool (&key (thread-count *thread-count*))
;;   (when (and (boundp '*thread-pool*)
;;              *thread-pool*)
;;     (error "Thread pool is already running"))
;;   (let ((tp (make-thread-pool
;;              :mutex (sb-thread:make-mutex)
;;              :cond (sb-thread:make-waitqueue))))
;;       (setf (thread-pool-threads tp)
;;             (loop for i below thread-count
;;                collect (sb-thread:make-thread (thread-pool-function tp)
;;                                               :name (format nil "thread-pool-~D" i))))
;;       (setq *thread-pool* tp)))

;; (defun stop-thread-pool (&key (thread-pool *thread-pool*) (wait t))
;;   (when thread-pool
;;     (with-sync-unit (thread-pool)
;;       (setf (thread-pool-exit thread-pool) t)
;;       (notify-sync-unit thread-pool))
;;     (when wait
;;       (map nil #'sb-thread:join-thread (thread-pool-threads thread-pool)))
;;     (when (eq thread-pool *thread-pool*)
;;       (setq *thread-pool* nil))))
