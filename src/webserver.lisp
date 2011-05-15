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
    (node edit_post add_comment edit_comment uri https error)
  (setf (hunchentoot:content-type*) "text/html")
  (acond (node (render-web-node node uri https))
         ((login-screen uri https))
         (edit_post (edit-post edit_post uri https))
         (add_comment (add-comment add_comment uri https))
         (edit_comment (edit-comment edit_comment uri https))
         (error (error-page error))
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
    (uri https node-num title alias published promoted body
         input-format preview submit delete)
  (submit-post uri https
               :node-num node-num
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
        (let ((plist (list :username username
                           :errmsg errmsg
                           :hidden-values `((:name "query-string"
                                                   :value ,query-string)))))
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

(defconstant $no-post-permission 1)
(defconstant $unknown-post-number 2)
(defconstant $no-add-or-edit-permission 3)

(defparameter *error-alist*
  `((,$no-post-permission . "You don't have permission to submit posts")
    (,$unknown-post-number . "Unknown post number.")
    (,$no-add-or-edit-permission . "You don't have permission to add or edit posts")))

(defun error-url (uri https errnum)
  (format nil "~aadmin/?error=~d" (compute-base-and-home uri https) errnum))

(defun redirect-to-error-page (uri https errnum)
  (hunchentoot:redirect (error-url uri https errnum)))

(defun error-page (errnum)
  (let* ((db (get-port-db))
         (errmsg (or (cdr (assoc (ignore-errors (parse-integer errnum))
                                 *error-alist*))
                     "Unknown error"))
         (plist (list :home ".."
                      :title "Error"
                      :errnum errnum
                      :errmsg errmsg)))
    (render-template ".error.tmpl" plist :data-db db)))

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
        (setf plist (list :posts
                          (list plist)
                          :page-title (getf plist :title)))
        (multiple-value-bind (base home) (compute-base-and-home uri https)
          (setf (getf plist :home) home
                (getf plist :base) base
                (getf plist :permalink) alias))
        (render-template post-template-name plist :data-db data-db)))))

(defun redirect-uri (uri https)
  (if uri
      (format nil "~a://~a" (if https "https" "http") uri)
      "http://google.com/search?&q=loser"))

(defun node-format-to-edit-post-plist (format)
  (cond ((eql format $filtered-html-format) '(:filtered-html t))
        ((eql format $full-html-format) '(:full-html t))
        ((eql format $raw-html-format) '(:raw-html t))))

(defparameter *format-name-to-number-alist*
  `(("filtered-html" . ,$filtered-html-format)
    ("full-html" . ,$full-html-format)
    ("raw-html" . ,$raw-html-format)))

(defun node-format-name-to-number (format-name)
  (or (cdr (assoc format-name *format-name-to-number-alist*
                  :test #'equal))
      1))

;; Node is a post node, as returned by read-node, or a node number, or
;; a list of category numbers (identified by the first element of the
;; list being an integer).
(defun node-to-edit-post-category-options (node &optional (db *data-db*))
  (when (integerp node)
    (setf node (read-node node db)))
  (let ((node-cats (if (or (null node) (integerp (car node)))
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
       collect (list* :value tid :label name
                      (and selected '(:selected t))))))

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
    (when (and node-num (not node))
      (return-from edit-post
        (redirect-to-error-page uri https $unknown-post-number)))
    (unless (or (memq :admin permissions)
                (memq :poster permissions))
      (return-from edit-post
        (redirect-to-error-page uri https $no-add-or-edit-permission)))
    (multiple-value-bind (base home) (compute-base-and-home uri https)
      (setf plist (list* :node-num node-num
                        :home home
                        :base base
                        :author (efh author)
                        :post-time (unix-time-to-rfc-1123-string created)
                        :title (efh title)
                        :alias (efh alias)
                        :published (eql status 1)
                        :promoted (eql promote 1)
                        :body (efh body)
                        :category-options (node-to-edit-post-category-options
                                           node db)
                        (node-format-to-edit-post-plist format))))
    (render-template ".edit-post.tmpl" plist :data-db db)))

(defvar *node-save-lock*
  (bt:make-recursive-lock "*node-save-lock*"))

(defmacro with-node-save-lock (&body body)
  `(bt:with-recursive-lock-held (*node-save-lock*)
     ,@body))

(defun save-updated-node (node &key
                          (data-db *data-db*)
                          (site-db *site-db*)
                          title uid body
                          created alias categories
                          (status 1)
                          (promote 1)
                          (format $filtered-html-format))
  (check-type title string)
  (check-type uid integer)
  (check-type body string)
  (check-type created (or null integer))
  (check-type alias (or null string))
  (check-type categories list)
  (check-type status integer)
  (check-type promote integer)
  (assert (member format *valid-post-format-values*))
  (assert (not (or (blankp title) (blankp uid) (blankp body))))
  (with-node-save-lock
    (let ((new-p (null node))
          (now (get-unix-time))
          (new-alias-p nil)
          (delete-aliases-p nil))
      (setf alias (compute-new-alias title alias data-db))
      (when (blankp created)
        (setf created now))
      (cond (new-p
             (setf node (list :aliases (list alias)
                              :nid (allocate-nid data-db)
                              :title title
                              :uid uid
                              :status status
                              :created created
                              :changed now
                              :promote promote
                              :body body
                              :format format)
                   new-alias-p t))
            (t (setf (getf node :changed) now)
               (let ((aliases (getf node :aliases)))
                 (unless (string= alias (car aliases))
                   (setf aliases (delete alias aliases :test #'string=))
                   (push alias aliases)
                   (setf (getf node :aliases) aliases)
                   (setf new-alias-p t)))
               (unless (string= title (getf node :title))
                 (setf (getf node :title) title
                       new-alias-p t))
               (unless (eql status (getf node :status))
                 (setf (getf node :status) status
                       new-alias-p t)
                 (unless (eql status 1)
                   (setf delete-aliases-p t)))
               (setf (getf node :changed) now
                     (getf node :promote) promote
                     (getf node :body) body
                     (getf node :format) format)
               (let ((old-categories
                      (loop for (cat) on (getf node :cat-neighbors) by #'cddr
                         collect cat)))
                 (unless (null (set-difference categories old-categories))
                   (setf node (update-node-categories
                               node categories old-categories data-db))))))
      (setf (read-node (getf node :nid) data-db) node)
      (cond (delete-aliases-p
             (remove-node-from-site node :data-db data-db :site-db site-db))
            (t (render-node node :data-db data-db :site-db site-db)))
      (when new-alias-p
        (update-node-year-and-month-pages node :data-db data-db :site-db site-db))
      ;; Don't always have to do this, but figuring out when
      ;; is harder than just doing it.
      (render-site-index :data-db data-db :site-db site-db)
      alias)))

(defun submit-post (uri https &key node-num title alias published
                    promoted body input-format preview submit delete)
  (let* ((session hunchentoot:*session*)
         (db (get-port-db))
         (uid (uid-of session))
         (user (read-user uid db))
         (author (getf user :name))
         (permissions (getf user :permissions))
         (node (read-node node-num db))
         (created (getf node :created))
         (post-time (and created (unix-time-to-rfc-1123-string created)))
         (promote (if (blankp promoted) 0 1))
         (status (if (blankp published) 0 1))
         (categories (mapcar 'parse-integer
                             (hunchentoot::compute-parameter
                              "categories" 'list :both)))
         (format (node-format-name-to-number input-format))
         (errmsg nil)
         plist)
    (cond ((blankp title)
           (setf errmsg "Title may not be blank")
           (when node
             (setf title (getf node :title))))
          ((blankp body)
           (setf errmsg "Body may not be blank")
           (when node
             (setf body (getf node :body)))))
    (when (and node-num (not node))
      (return-from submit-post
        (redirect-to-error-page uri https $unknown-post-number)))
    (unless (or (memq :admin permissions)
                (memq :poster permissions))
      (return-from submit-post
        (redirect-to-error-page uri https $no-add-or-edit-permission)))
    (cond ((or preview errmsg)
           (multiple-value-bind (base home) (compute-base-and-home uri https)
             (setf plist
                   (list* :node-num node-num
                          :home home
                          :base base
                          :errmsg (efh errmsg)
                          :author (efh author)
                          :post-time post-time
                          :title (efh title)
                          :alias (efh alias)
                          :published (eql status 1)
                          :promoted (eql promote 1)
                          :body (efh body)
                          :category-options (node-to-edit-post-category-options
                                             categories db)
                          (node-format-to-edit-post-plist format)))
             (when preview
               (let* ((template-name (get-post-template-name db))
                      (template (get-style-file template-name db))
                      (node-plist
                       (make-node-plist
                        (list :nid node-num
                              :uid uid
                              :title title
                              :aliases (list alias)
                              :created created
                              :status 1
                              :body body
                              :format format
                              :home home)
                        :comments-p nil
                        :data-db db)))
                 (setf (getf node-plist :no-editing) t
                       (getf node-plist :permalink) (efh alias))
                 (setf node-plist (list :posts (list node-plist)))
                 (setf (getf plist :preview)
                       (fill-and-print-to-string template node-plist)))))
           (render-template ".edit-post.tmpl" plist :data-db db))
          (submit
           (setf alias
                 (save-updated-node node
                                    :data-db db
                                    :alias alias
                                    :title title
                                    :uid uid
                                    :created created
                                    :status status
                                    :promote promote
                                    :body body
                                    :categories categories
                                    :format format))
           (let ((base (compute-base-and-home uri https)))
             (hunchentoot:redirect (format nil "~a~a" base alias))))
          (delete "Delete not done yet"))))

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
