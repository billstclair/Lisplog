; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CCL interface functions 
;;;

(in-package :fsdb)

(defun make-lock (&optional name)
  (ccl:make-lock name))

(defun grab-lock (lock)
  (ccl:grab-lock lock))

(defun release-lock (lock)
  (ccl:release-lock lock))

(defmacro with-lock-grabbed ((lock &optional (whostate "Lock")) &body body)
  `(ccl:with-lock-grabbed (,lock ,whostate) ,@body))

;;;
;;; Semaphores
;;;

(defun make-semaphore ()
  (ccl:make-semaphore))

(defun signal-semaphore (semaphore)
  (ccl:signal-semaphore semaphore))

(defun wait-on-semaphore (semaphore)
  (ccl:wait-on-semaphore semaphore))

;;;
;;; Weak hash tables
;;;

(defun make-weak-hash-table ()
  (make-hash-table :test 'eq :weak t))

;;;
;;; Processes
;;;

(defun current-process ()
  ccl:*current-process*)

(defun all-processes ()
  (ccl:all-processes))

(defun process-run-function (name function &rest args)
  (declare (dynamic-extent args))
  (apply #'ccl:process-run-function name function args))

(defun process-wait (whostate function &rest args)
  (declare (dynamic-extent args))
  (apply #'ccl:process-wait whostate function args))

;;;
;;; Directories
;;;

(defun create-directory (dir &key mode)
  (if mode
      (ccl:create-directory dir :mode mode)
      (ccl:create-directory dir)))

(defun recursive-delete-directory (path &rest rest &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (apply #'ccl::recursive-delete-directory path rest))

(defun ensure-directory-pathname (path)
  (ccl::ensure-directory-pathname path))
              

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
