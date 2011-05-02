; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisplog web server
;;;

(in-package :lisplog)

(defmacro with-site-db ((&optional (db *data-db*)) &body body)
  `(let ((*data-db* ,db))
     (with-settings ()
       (let ((*site-db* (fsdb:make-fsdb (get-setting :site-directory))))
         ,@body))))

(defvar *webserver-db* nil)
(defvar *webserver-acceptor* nil)

(defun start (&optional (db *data-db*))
  (when *webserver-acceptor*
    (error "Hunchentoot already started"))
  (when (stringp db) (setf db (fsdb:make-fsdb db)))
  (with-site-db (db)
    (let ((port (or (get-setting :port) (error "No port setting"))))
      (prog1
          (setf hunchentoot:*show-lisp-errors-p* t)
          (setf *webserver-acceptor*
                (hunchentoot:start
                 (make-instance 'hunchentoot:acceptor :port port)))
        (setf *webserver-db* db)))))

(defun stop ()
  (when *webserver-acceptor*
    (hunchentoot:stop *webserver-acceptor*)
    (setf *webserver-acceptor* nil
          *webserver-db* nil)))

(defun compute-base-and-home (uri https &optional alias)
  (declare (ignore alias))              ;do later
  (let ((base (format nil "http~a://~a"
                      (if (equal https "on") "s" "")
                      (subseq uri 0 (- (length uri) 6)))) ;remove "admin/"
        )
    (values base ".")))

(defun render-node-string (node uri https &key alias (data-db *data-db*))
  (with-settings ()
    (let* ((plist (make-node-plist node :data-db data-db))
           (post-template-name (get-post-template-name data-db)))
      (when plist
        (unless alias
          (setf alias (car (getf plist :aliases))))
        (setf plist `(:posts
                      (,plist)
                      :page-title ,(getf plist :title)
                      ,@(compute-history-plist node data-db)))
        (multiple-value-bind (base home) (compute-base-and-home uri https)
          (setf (getf plist :home) home
                (getf plist :base) base))
        (setf (getf plist :permalink) alias)
        (render-template post-template-name plist :data-db data-db)))))

(hunchentoot:define-easy-handler (handle-admin :uri "/") (node uri https)
  (setf (hunchentoot:content-type*) "text/html")
  (acond ((and node (ignore-errors (parse-integer node)))
          (render-node-string it uri https
                              :data-db *webserver-db*))
         (t (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))))

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
