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

;;;
;;; URL handlers
;;; You will usually get here via a RewriteRule in your blogs .htaccess file
;;; I use the "admin" dir to trigger that rule.
;;; This code would work with another trigger, however.
;;;

;; <baseurl>/admin/?command=value
(hunchentoot:define-easy-handler (handle-admin :uri "/")
    (node edit_post add_comment edit_comment uri https)
  (setf (hunchentoot:content-type*) "text/html")
  (acond (node (render-web-node node uri https))
         (edit_post (edit-post edit_post uri https))
         (add_comment (add-comment add_comment uri https))
         (edit_comment (edit-comment edit_comment uri https))
         (t (not-found))))

;; <baseurl>/admin/settings
(hunchentoot:define-easy-handler (settings :uri "/settings") (uri https)
  (format nil "settings, uri: ~s, https: ~s" uri https))

;; <baseurl>/admin/add_post
(hunchentoot:define-easy-handler (add-post :uri "/add_post") (uri https)
  (format nil "add post, uri: ~s, https: ~s" uri https))

;;;
;;; Implementation of URL handlers
;;;

(defun compute-base-and-home (uri https &optional alias)
  (declare (ignore alias))              ;do later
  (let ((base (format nil "http~a://~a"
                      (if (equal https "on") "s" "")
                      (subseq uri 0 (- (length uri) 6)))) ;remove "admin/"
        )
    (values base ".")))

;; <baseurl>/admin/?node=<node-num>
(defun render-web-node (node-num uri https &key alias (data-db *webserver-db*))
  (unless (setf node-num (ignore-errors (parse-integer node-num)))
    (return-from render-web-node (not-found)))
  (with-settings ()
    (let* ((plist (make-node-plist node-num :data-db data-db))
           (post-template-name (get-post-template-name data-db)))
      (when plist
        (unless alias
          (setf alias (car (getf plist :aliases))))
        (setf plist `(:posts
                      (,plist)
                      :page-title ,(getf plist :title)
                      ,@(compute-history-plist node-num data-db)))
        (multiple-value-bind (base home) (compute-base-and-home uri https)
          (setf (getf plist :home) home
                (getf plist :base) base))
        (setf (getf plist :permalink) alias)
        (render-template post-template-name plist :data-db data-db)))))

(defun not-found ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  "404")

;; <baseurl>/admin/?edit_post=<node-num>
(defun edit-post (node-num uri https)
  (format nil "edit post, node: ~s, uri: ~s, https: ~s" node-num uri https))

;; <baseurl>/admin/?add_comment=<node-num>
(defun add-comment (node-num uri https)
  (format nil "add comment, node: ~s, uri: ~s, https: ~s" node-num uri https))

;; <baseurl>/admin/?edit_comment=<comment-num>
(defun edit-comment (comment-num uri https)
  (format nil "edit comment, comment: ~s, uri: ~s, https: ~s" comment-num uri https))

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
