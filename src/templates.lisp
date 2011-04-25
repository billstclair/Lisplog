; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn data into web pages using styles
;;;

(in-package :lisplog)

;;;
;;; DBs for directories
;;;

(defparameter *styles-directory*
  (merge-pathnames "styles/" *lisplog-home*))

(defparameter *lisplog-db* (fsdb:make-fsdb *lisplog-home*))
(defparameter *styles-db* (fsdb:make-fsdb *styles-directory*))

(defparameter *data-db*
  ;; This is the default location, for development
  (fsdb:db-subdir *lisplog-db* $DATA))

(defparameter *site-db*
  ;; This is the default location, for development
  (fsdb:db-subdir *lisplog-db* $SITE))

;; Bound during template operations
;; This is just a default, for development
(defparameter *style-db*
  (fsdb:db-subdir *styles-db* "etwof"))

(defparameter *style-index-file* ".index.html")

;;;
;;; Accessing styles and site files
;;;

(defun get-style-file (file &optional (style-db *style-db*))
  (fsdb:db-get style-db file))

(defun write-site-file (path contents &optional (site-db *site-db*))
  (setf (fsdb:db-get site-db path) contents)
  nil)

(defun initialize-site (&key (style-db *style-db*) (site-db *site-db*))
  "Copy all files not beginning with '.' from style-db to site-db"
  (labels ((copy-dir (path)
             (dolist (file (fsdb:db-contents style-db path))
               (unless (eql #\. (elt file 0))
                 (let ((path (fsdb:append-db-keys path file)))
                   (if (fsdb:db-dir-p style-db path)
                       (copy-dir path)
                       (setf (fsdb:db-get site-db path)
                             (fsdb:db-get style-db path))))))))
    (copy-dir ".")))

;;;
;;; Settings
;;;

;; Bound during template operations
(defvar *settings* nil)

(defun get-setting (key &optional (settings *settings*))
  (getf settings key))

(defun (setf get-setting) (value key &optional (settings *settings*))
  (setf (getf settings key) value))

(defun read-settings (&optional data-db)
  (unless data-db
    (setf data-db *data-db*))
  (node-get data-db nil $SETTINGS :subdirs-p nil))

(defun (setf read-settings) (value &optional data-db)
  (unless data-db
    (setf data-db *data-db*))
  (setf (node-get data-db nil $SETTINGS :subdirs-p nil) value))

(defmacro with-settings ((&optional data-db) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (*settings*) ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-settings #',thunk ,data-db))))

(defun call-with-settings (thunk data-db)
  (funcall thunk (read-settings data-db)))



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
