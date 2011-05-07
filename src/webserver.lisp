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

(defun start (&optional (db *data-db*))
  (when (stringp db) (setf db (fsdb:make-fsdb db)))
  (with-site-db (db)
    (let ((port (or (get-setting :port) (error "No port setting"))))
      (when (get-port-acceptor port)
        (error "Hunchentoot already started"))
      (prog1
          (setf hunchentoot:*show-lisp-errors-p* t)
          (setf (get-port-acceptor port)
                (hunchentoot:start
                 (make-instance 'lisplog-acceptor :port port)))
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

;; <baseurl>/admin/submit_post
(hunchentoot:define-easy-handler (handle-submit-post :uri "/submit_post")
    (uri https node-num author post-time title alias published promoted body
         input-format preview submit delete)
  (submit-post uri https
               :node-num node-num
               :author author
               :post-time post-time
               :title title
               :alias alias
               :published published
               :promoted promoted
               :body body
               :input-format input-format
               :preview preview
               :submit submit
               :delete delete))

;; <baseurl>/admin/login
(hunchentoot:define-easy-handler (handle-login :uri "/login")
    (query-string username password uri https)
  (login query-string username password uri https))

;;;
;;; Login and registration
;;;

;; Would be nice to show month history, but need to pass a node in here
;; to do that.
(defun login-screen (uri https &key
                     errmsg
                     (username "")
                     (query-string (hunchentoot:query-string*)))
  (let ((session hunchentoot:*session*)
        (db (get-port-db)))
    (unless (and session (uid-of session))
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
  (let* ((session (start-session))
         (db (get-port-db))
         (user (get-user-by-name username db)))
    (cond ((not (and user
                     (equal (md5 password) (getf user :pass))))
           (login-screen uri https
                         :errmsg "Unknown user or wrong password"
                         :username username
                         :query-string query-string))
          (t (setf (uid-of session) (getf user :uid))
             (write-session session db)
             (hunchentoot:redirect (login-redirect-uri query-string))))))

;;;
;;; Implementation of URL handlers
;;;

(defun compute-base-and-home (uri https &optional alias)
  (declare (ignore alias))              ;do later
  (let* ((pos (search "/admin/" uri :from-end t :test #'equal))
         (base (format nil "http~a://~a"
                       (if (equal https "on") "s" "")
                       (subseq uri 0 (1+ pos)))))
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

(defun redirect-uri (uri https)
  (if uri
      (format nil "~a://~a" (if https "https" "http") uri)
      "http://google.com/search?&q=loser"))

(defun node-format-to-edit-post-plist (format)
  (cond ((eql format 1) '(:filtered-html t))
        ((eql format 3) '(:full-html t))
        ((eql format 5) '(:raw-html t))))

(defun node-to-edit-post-category-options (node &optional (db *data-db*))
  (when (integerp node)
    (setf node (read-node node db)))
  (let ((node-cats (if (listp node)
                       node
                       (loop for (cat) on (getf node :cat-neighbors) by #'cddr
                          collect cat)))
        (all-cats (sort (let (cats) (do-categories (cat db) (push cat cats)) cats)
                        'string-lessp
                        :key (lambda (x) (getf x :name)))))
    (loop for cat in all-cats
       for tid = (getf cat :tid)
       for name = (getf cat :name)
       for selected = (member tid node-cats)
       collect `(:value ,tid :label ,name
                 ,@(and selected '(:selected t))))))

;; <baseurl>/admin/?edit_post=<node-num>
(defun edit-post (node-num uri https)
  (let* ((session hunchentoot:*session*)
         (db (get-port-db))
         (uid (uid-of session))
         (user (read-user uid db))
         (permissions (getf user :permissions))
         (author (getf user :name))
         (node (read-node node-num db))
         (alias (car (getf node :aliases)))
         (title (getf node :title))
         (created (getf node :created))
         (promote (getf node :promote))
         (status (getf node :status))
         (body (getf node :body))
         (format (getf node :format))
         plist)
    (unless (and node
                 (or (memq :admin permissions)
                     (memq :poster permissions)))
      (return-from edit-post
        (hunchentoot:redirect (redirect-uri uri https))))
    (multiple-value-bind (base home) (compute-base-and-home uri https)
      (setf plist `(:node-num ,node-num
                    :home ,home
                    :base ,base
                    :author ,(efh author)
                    :post-time ,(unix-time-to-rfc-1123-string created)
                    :title ,(efh title)
                    :alias ,(efh alias)
                    :published ,(eql status 1)
                    :promoted ,(eql promote 1)
                    :body ,(efh body)
                    ,@(node-format-to-edit-post-plist format)
                    :category-options ,(node-to-edit-post-category-options node db)
                    ,@(compute-months-and-years-link-plist nil nil db))))
    (render-template ".edit-post.tmpl" plist :data-db db)))

(defun submit-post (uri https &key node-num author post-time title alias published
                    promoted body input-format preview submit delete)
  (let* ((session hunchentoot:*session*)
         (db (get-port-db))
         (uid (uid-of session))
         (user (read-user uid db))
         (permissions (getf user :permissions))
         (node (read-node node-num db))
         (errmsg nil)
         (categories (mapcar 'parse-integer
                             (hunchentoot::compute-parameter
                              "categories" 'list :both)))
         plist)
    (unless (and node
                 (or (memq :admin permissions)
                     (memq :poster permissions)))
      (setf errmsg "You don't have permission to submit posts"))
    (multiple-value-bind (base home) (compute-base-and-home uri https)
      (setf plist `(:node-num ,node-num
                    :errmsg ,errmsg
                    :home ,home
                    :base ,base
                    :author ,(efh author)
                    :post-time ,(efh post-time)
                    :title ,(efh title)
                    :alias ,(efh alias)
                    :published ,(not (blankp published))
                    :promoted ,(not (blankp promoted))
                    :body ,(efh body)
                    ,@(node-format-to-edit-post-plist
                       (cdr (assoc input-format
                                   '(("filtered-html" . 1)
                                     ("full-html" . 3)
                                     ("raw-html" . 5))
                                   :test #'equal)))
                    :category-options ,(node-to-edit-post-category-options
                                        categories db)
                    ,@(compute-months-and-years-link-plist nil nil db))))
    (render-template ".edit-post.tmpl" plist :data-db db)))

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
