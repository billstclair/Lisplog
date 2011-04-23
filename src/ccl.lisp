; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CCL interface functions 
;;;

(in-package :lisplog)

;; This noticeably speeds things up
(ccl:egc nil)

(defun run-program (program args &key input output (wait (not (eq output :stream))))
  (let ((proc (ccl:run-program program args :input input :output output :wait wait)))
    (if (eq output :stream)
        (ccl:external-process-output-stream proc)
        proc)))

(defun quit (&optional (exit-status 0))
  (ccl:quit exit-status))

(defun df (x) (disassemble x))

(defun arglist (x &optional include-bindings)
  (ccl:arglist x include-bindings))

(defun make-weak-hash-table ()
  (make-hash-table :test 'eq :weak t))

(defun gc ()
  (ccl:gc))

(defun backtrace-string ()
  (let ((bt (ccl::backtrace-as-list))
        (i 0))
    (with-output-to-string (s)
      (dolist (b (cdr bt))              ;Don't print the backtrace-string frame
        (format s "~d:~{ ~a~}~%" i b)
        (incf i)))))

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
