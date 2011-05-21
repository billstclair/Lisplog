; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application layer on top of FSDB
;;;

(in-package :lisplog)

(defun node-path (file dir &optional subdirs-p)
  (when (integerp file) (setf file (princ-to-string file)))
  (let* ((len (min 2 (length file)))
         (subdir (and subdirs-p
                      (eql len 2)
                      (list (subseq file 0 len)))))
    (when (eql len 1)
      (setf file (strcat "0" file)))
    (append (ensure-list dir) subdir (list file))))

(defun sexp-get (db dir file &key (subdirs-p t))
  (let ((str (apply #'fsdb:db-get db (node-path file dir subdirs-p))))
    (and str (read-from-string str))))

(defun (setf sexp-get) (alist db dir file &key (subdirs-p t))
  (let ((key (apply #'fsdb:append-db-keys (node-path file dir subdirs-p))))
    (setf (fsdb:db-get db key)
          (and alist (with-output-to-string (s) (pprint alist s))))))

(defun sexp-probe (db dir file &key (subdirs-p t))
  (apply #'fsdb:db-probe db (node-path file dir subdirs-p)))


;; You have to side-affect the NODE-VAR
(defmacro updating-node ((node-var db dir file &key (subdirs-p t)) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,node-var) ,@body ,node-var))
       (call-updating-node #',thunk ,db ,file ,dir ,subdirs-p))))

(defun call-updating-node (thunk db file dir subdirs-p)
  (let ((node (sexp-get db dir file :subdirs-p subdirs-p)))
    (when (setf node (funcall thunk node))
      (setf (sexp-get db dir file :subdirs-p subdirs-p) node))))


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
