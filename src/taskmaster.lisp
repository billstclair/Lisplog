; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A Hunchentoot taskmaster that limits the number of worker threads
;;;

(in-package :lisplog)

(defparameter *hunchentoot-worker-thread-limit* 10)

(defvar *eager-future-pool* nil)

(defun eager-future-pool ()
  (or *eager-future-pool*
      (let ((pool (make-instance 'eager-future:fixed-fifo-thread-pool)))
        (setf (eager-future:thread-limit pool)
              *hunchentoot-worker-thread-limit*)
        (setf *eager-future-pool* pool))))

(defclass limited-thread-taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  ((thread-pool :initform (eager-future-pool)
                :accessor thread-pool-of)))

(defmethod hunchentoot:handle-incoming-connection
    ((taskmaster limited-thread-taskmaster) socket)
  (let ((eager-future:*thread-pool* (thread-pool-of taskmaster)))
    (eager-future:pexec
      (let ((thread (bt:current-thread)))
        (declare (ignorable thread))
        #+ccl
        (setf (ccl:process-name thread)
              (format nil "Hunchentoot worker \(client: ~A)"
                      (hunchentoot::client-as-string socket)))
        (unwind-protect
             (hunchentoot:process-connection
              (hunchentoot:taskmaster-acceptor taskmaster)
              socket)
          #+ccl
          (setf (ccl:process-name thread) "Hunchentoot worker (idle)"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2011 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
