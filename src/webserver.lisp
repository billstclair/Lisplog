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

;; This allows serving multiple weblogs from a single lisp image.
;; That will work if they take data from separate data directories,
;; and render to separate site directories.
;; It will probably fail in mysterious ways if you try to
;; share a data or site directory between two ports.

(defun get-port-db (&optional (port (hunchentoot:acceptor-port
                                     hunchentoot:*acceptor*)))
  (cdr (assoc port *port-db-alist*)))

(defun (setf get-port-db) (db &optional (port (hunchentoot:acceptor-port
                                               hunchentoot:*acceptor*)))
  (let ((cell (assoc port *port-db-alist*)))
    (if cell
        (setf (cdr cell) db)
        (push (cons port db) *port-db-alist*))))

(defun get-port-acceptor (&optional (port (hunchentoot:acceptor-port
                                           hunchentoot:*acceptor*)))
  (cdr (assoc port *port-acceptor-alist*)))

(defun (setf get-port-acceptor) (acceptor &optional
                                 (port (hunchentoot:acceptor-port
                                        hunchentoot:*acceptor*)))
  (let ((cell (assoc port *port-acceptor-alist*)))
    (if cell
        (setf (cdr cell) acceptor)
        (push (cons port acceptor) *port-acceptor-alist*)))
  acceptor)

(defun start (&optional (db *data-db*))
  (when *webserver-acceptor*
    (error "Hunchentoot already started"))
  (when (stringp db) (setf db (fsdb:make-fsdb db)))
  (with-site-db (db)
    (let ((port (or (get-setting :port) (error "No port setting"))))
      (prog1
          (setf hunchentoot:*show-lisp-errors-p* t)
          (setf (get-port-acceptor port)
                (hunchentoot:start
                 (make-instance 'hunchentoot:acceptor :port port)))
        (setf (get-port-db port) db)))))

(defun stop (&key port (db *data-db*))
  (unless port
    (when db
      (with-settings (db) (setf port (get-setting :port))))
    (unless port
      (cond ((and *port-db-alist* (null (cdr *port-db-alist*)))
             (setf db (cadr *port-db-alist*)))
            (t (error "Can't determine port")))))
  (awhen (get-port-acceptor port)
    (hunchentoot:stop it)
    (setf (get-port-acceptor port) nil
          (get-port-db port) nil)))

;;;
;;; URL handlers
;;; You will usually get here via a RewriteRule in your blog's .htaccess file
;;; I use the "admin" dir to trigger that rule.
;;; This code would work with another trigger, however.
;;;

;; <baseurl>/admin/?command=value
(hunchentoot:define-easy-handler (handle-admin :uri "/")
    (node edit_post add_comment edit_comment uri https)
  (setf (hunchentoot:content-type*) "text/html")
  (acond (node (render-web-node node uri https))
         ((login-screen uri https))
         (edit_post (edit-post edit_post uri https))
         (add_comment (add-comment add_comment uri https))
         (edit_comment (edit-comment edit_comment uri https))
         (t (not-found))))

;; <baseurl>/admin/settings
(hunchentoot:define-easy-handler (handle-settings :uri "/settings") (uri https)
  (or (login-screen uri https)
      (settings uri https)))

;; <baseurl>/admin/add_post
(hunchentoot:define-easy-handler (handle-add-post :uri "/add_post") (uri https)
  (or (login-screen uri https)
      (add-post uri https)))

;; <baseurl>/admin/login
(hunchentoot:define-easy-handler (handle-login :uri "/login")
    (query-string username password uri https)
  (login query-string username password uri https))

;;;
;;; Login and registration
;;;

(defun session-user-num (&optional (session hunchentoot:*session*))
  (hunchentoot:session-value session))

(defun (setf session-user-num) (user-num &optional (session hunchentoot:*session*))
  (setf (hunchentoot:session-value session) user-num))

;; Would be nice to show month history, but need to pass a node in here
;; to do that.
(defun login-screen (uri https &key
                     errmsg
                     (username "")
                     (query-string (hunchentoot:query-string*)))
  (let ((session (hunchentoot:start-session))
        (db (get-port-db)))
    (unless (session-user-num session)
      (with-settings (db)
        (let ((plist `(:username ,username
                       :errmsg ,errmsg
                       :hidden-values ((:name "query-string"
                                        :value ,query-string))
                       ,@(compute-months-and-years-link-plist nil nil db))))
          (multiple-value-bind (base home) (compute-base-and-home uri https)
            (setf (getf plist :home) home
                  (getf plist :base) base))
          (render-template ".login.tmpl" plist :data-db db))))))
          
(defun login-redirect-uri (query-string)
  (let* ((params (hunchentoot::form-url-encoded-list-to-alist
                  (cl-ppcre::split "&" query-string)))
         (uri (cdr (assoc "uri" params :test #'equal)))
         (https (cdr (assoc "https" params :test #'equal)))
         (first t))
    (setf uri (strcat (if (equal https "on") "https" "http")
                      "://"
                      uri))
    (dolist (cell params)
      (unless (member (car cell) '("uri" "https") :test #'equal)
        (setf uri (strcat uri
                          (if first "?" "&")
                          (car cell)
                          "="
                          (hunchentoot:url-encode (cdr cell)))
              first nil)))
    uri))

(defun login (query-string username password uri https)
  (let* ((session (hunchentoot:start-session))
         (db (get-port-db))
         (user (get-user-by-name username db)))
    (cond ((not (and user
                     (equal (md5 password) (getf user :pass))))
           (login-screen uri https
                         :errmsg "Unknown user or wrong password"
                         :username username
                         :query-string query-string))
          (t (setf (session-user-num session) (getf user :uid))
             (hunchentoot:redirect (login-redirect-uri query-string))))))

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

(defun not-found ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  "404")

;; <baseurl>/admin/?node=<node-num>
(defun render-web-node (node-num uri https &key alias (data-db (get-port-db)))
  (unless (setf node-num (ignore-errors (parse-integer node-num)))
    (return-from render-web-node (not-found)))
  (with-settings (data-db)
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

;; <baseurl>/admin/?edit_post=<node-num>
(defun edit-post (node-num uri https)
  (format nil "edit post, node: ~s, uri: ~s, https: ~s" node-num uri https))

;; <baseurl>/admin/?add_comment=<node-num>
(defun add-comment (node-num uri https)
  (format nil "add comment, node: ~s, uri: ~s, https: ~s" node-num uri https))

;; <baseurl>/admin/?edit_comment=<comment-num>
(defun edit-comment (comment-num uri https)
  (format nil "edit comment, comment: ~s, uri: ~s, https: ~s" comment-num uri https))

;; <baseurl>/admin/settings
(defun settings (uri https)
  (format nil "settings, uri: ~s, https: ~s" uri https))

;; <baseurl>/admin/add_post
(defun add-post (uri https)
  (format nil "add post, uri: ~s, https: ~s" uri https))


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
