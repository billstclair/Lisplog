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

(defun (setf sexp-get) (sexp db dir file &key (subdirs-p t))
  (let ((key (apply #'fsdb:append-db-keys (node-path file dir subdirs-p))))
    (setf (fsdb:db-get db key)
          (and sexp (with-output-to-string (s) (pprint sexp s))))
    sexp))

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

(defun map-nodes-or-comments (function nodes-or-comments &optional (db *data-db*))
  "Calls function with the plist for each node, in directory order"
  (flet ((get-node (path file)
           (let ((str (fsdb:db-get db path file)))
             (when str (read-from-string str)))))
    (declare (dynamic-extent #'get-node))
    (dolist (file (fsdb:db-contents db nodes-or-comments))
      (let ((path (fsdb:append-db-keys nodes-or-comments file)))
        (cond ((fsdb:db-dir-p db path)
               (dolist (file (fsdb:db-contents db path))
                 (funcall function (get-node path file))))
              (t (funcall function (get-node nodes-or-comments file))))))))

(defun map-nodes (function &optional (db *data-db*))
  (map-nodes-or-comments function $NODES db))

(defmacro do-nodes ((node &optional (db '*data-db*)) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block nil
       (flet ((,thunk (,node) ,@body))
         (declare (dynamic-extent #',thunk))
         (map-nodes #',thunk ,db)))))

(defun map-comments (function &optional (db *data-db*))
  (map-nodes-or-comments function $COMMENTS db))

(defmacro do-comments ((comment &optional (db '*data-db*)) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block nil
       (flet ((,thunk (,comment) ,@body))
         (declare (dynamic-extent #',thunk))
         (map-comments #',thunk ,db)))))

(defun last-n-active-comments (n &optional (db *data-db*))
  (let ((cid (read-cid db))
        (res nil)
        (cnt 0))
    (loop while (> cid 0)
       for comment = (read-comment cid db)
       while (< cnt n)
       do
         (when (eql 0 (getf comment :status))
           (push comment res)
           (incf cnt))
         (decf cid))
    (nreverse res)))

(defun map-users (function &optional (db *data-db*))
  (map-nodes-or-comments function $USERS db))

(defmacro do-users ((user &optional (db '*data-db*)) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block nil
       (flet ((,thunk (,user) ,@body))
         (declare (dynamic-extent #',thunk))
         (map-users #',thunk ,db)))))

(defun map-categories (function &optional (db *data-db*))
  (map-nodes-or-comments function $CATEGORIES db))

(defmacro do-categories ((cat &optional (db '*data-db*)) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block nil
       (flet ((,thunk (,cat) ,@body))
         (declare (dynamic-extent #',thunk))
         (map-categories #',thunk ,db)))))

(defmacro do-node-nums-before-time ((node-num unix-time &optional (db '*data-db*))
                                    &body body)
  (let ((thunk (gensym "THUNK")))
    `(block nil
       (flet ((,thunk (,node-num) ,@body))
         (declare (dynamic-extent #',thunk))
         (map-node-nums-before-time #',thunk ,unix-time ,db)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2011-2013 Bill St. Clair
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
