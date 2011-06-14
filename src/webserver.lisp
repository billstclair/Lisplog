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

;; Character encoding is actually a little wierd.
;; We're displaying as UTF-8, and the browser sends forms
;; to us encoded as UTF-8, but we receive latin-1
;; (see hunchentoot:*hunchentoot-default-external-format*),
;; and we read/write latin-1 from/to files (fsdb::file-get/put-contents).
;; Same sequence of bytes, but stored internally as latin-1.
;; It's actually less pain that way. Slime doesn't handle
;; UTF-8 over the wire.
(setf hunchentoot:*default-content-type*
      "text/html; charset=utf-8")

(defun start (&optional (db *data-db*))
  (when db
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
          (setf (get-port-db port) db))))))

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
    (node edit_post add_comment login edit_comment uri https error)
  (setf (hunchentoot:content-type*) "text/html")
  (cond (add_comment
         (or (and login (login-screen uri https))
             (add-comment add_comment uri https)))
        ((login-screen uri https))
        (node (render-web-node node uri https))
        (edit_post (edit-post edit_post uri https))
        (edit_comment (edit-comment edit_comment uri https))
        (error (error-page error))
        (t (not-found))))

;; <baseurl>/admin/settings
(hunchentoot:define-easy-handler (handle-settings :uri "/settings") (uri https)
  (or (login-screen uri https)
      (settings uri https)))

;; <baseurl>/admin/threads
(hunchentoot:define-easy-handler (handle-threads :uri "/threads") (uri https)
  (or (login-screen uri https)
      (threads uri https)))

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

;; <baseurl>/admin/submit_comment
(hunchentoot:define-easy-handler (handle-submit-comment :uri "/submit_comment")
    (uri https comment-num node-num author email homepage title published body
         captcha-response captcha-hidden
         input-format preview submit delete)
  (setf comment-num (ignore-errors (parse-integer comment-num))
        node-num (ignore-errors (parse-integer node-num)))
  (submit-comment
   uri https
   :comment-num comment-num
   :node-num node-num
   :author author
   :email email
   :homepage homepage
   :title title
   :published published
   :body body
   :input-format input-format
   :captcha-response captcha-response
   :captcha-hidden captcha-hidden
   :preview preview
   :submit submit
   :delete delete))

;; <baseurl>/admin/login
(hunchentoot:define-easy-handler (handle-login :uri "/login")
    (query-string username password uri https)
  (login query-string username password uri https))

(hunchentoot:define-easy-handler (handle-moderate :uri "/moderate") (uri https)
  (or (login-screen uri https)
      (moderate uri https)))

(hunchentoot:define-easy-handler (handle-submit-moderate :uri "/submit_moderate")
    (uri https submit)
  (or (login-screen uri https)
      (submit-moderate uri https :submit submit)))

(hunchentoot:define-easy-handler (handle-register :uri "/register")
    (uri https submit username email captcha-response captcha-hidden verify)
  (register uri https
            :submit submit
            :username username
            :email email
            :captcha-response captcha-response
            :captcha-hidden captcha-hidden
            :verify verify))

(hunchentoot:define-easy-handler (handle-profile :uri "/profile")
    (uri https verify oldpass newpass newpass2 email homepage submit)
  (or (and (not verify) (login-screen uri https))
      (profile uri https
               :verify verify
               :oldpass oldpass
               :newpass newpass
               :newpass2 newpass2
               :email email
               :homepage homepage
               :submit submit)))

(hunchentoot:define-easy-handler (handle-email-change :uri "/emailchange")
    (uri https verify)
  (or (login-screen uri https)
      (email-change uri https verify)))

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
          (render-template ".login.tmpl" plist
                           :add-index-comment-links-p t
                           :data-db db))))))

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

(defun make-new-session (uid &optional (db *data-db*))
  (let ((session (start-session)))
    (setf (uid-of session) uid)
    (write-session session db)
    session))

(defun login (query-string username password uri https)
  (let* ((db (get-port-db))
         (user (get-user-by-name username db)))
    (cond ((not (and user
                     (equal (md5 password) (getf user :pass))))
           (login-screen uri https
                         :errmsg "Unknown user or wrong password"
                         :username username
                         :query-string query-string))
          (t (make-new-session (getf user :uid))
             (hunchentoot:redirect (login-redirect-uri query-string))))))

(defconstant $no-post-permission 1)
(defconstant $unknown-post-number 2)
(defconstant $no-add-or-edit-permission 3)
(defconstant $cant-delete 4)
(defconstant $no-edit-comment-permission 5)
(defconstant $unknown-comment-number 6)
(defconstant $name-may-not-be-blank 7)
(defconstant $no-moderation-permission 8)
(defconstant $bad-registration 9)
(defconstant $used-registration 10)
(defconstant $logged-in-registration 11)
(defconstant $bad-email-change 12)

(defparameter *error-alist*
  `((,$no-post-permission . "You don't have permission to submit posts.")
    (,$unknown-post-number . "Unknown post number.")
    (,$no-add-or-edit-permission . "You don't have permission to add or edit posts.")
    (,$cant-delete . "Can't delete, no node-num.")
    (,$no-edit-comment-permission . "You may only edit your own comments.")
    (,$unknown-comment-number . "Unknown comment number.")
    (,$name-may-not-be-blank . "Name may not be blank.")
    (,$no-moderation-permission . "You do not have permission to do moderation.")
    (,$bad-registration . "Bad registration link.")
    (,$used-registration . "Registration link already used.")
    (,$logged-in-registration . "You must logout before registering a new account.")
    (,$bad-email-change . "Invalid email change link.")))

(defun registration-secret (&optional (db *data-db*))
  (or (sexp-get db $CAPTCHA $SECRET :subdirs-p nil)
      (setf (sexp-get db $CAPTCHA $SECRET :subdirs-p nil)
            (cl-crypto:get-random-bits 160))))

(defun compute-registration-hash (str &optional (db *data-db*))
  (cl-crypto:sha1
   (format nil "~x"
           (logxor (registration-secret db)
                   (parse-integer (cl-crypto:sha1 str) :radix 16)))))

(defun encode-registration (username email &optional (db *data-db*))
  (let* ((str (format nil "~a|~a" username email))
         (hash (compute-registration-hash str db)))
    (format nil "~a|~a"
            (cl-base64:string-to-base64-string str)
            hash)))

(defun decode-registration (str &optional (db *data-db*))
  (let ((parts (split-sequence:split-sequence #\| str)))
    (unless (eql (length parts) 2)
      (error "Malformed registration value"))
    (destructuring-bind (str hash) parts
      (setf str (cl-base64:base64-string-to-string str))
      (unless (equal hash (compute-registration-hash str db))
        (error "Mismatched registration hash"))
      (setf parts (split-sequence:split-sequence #\| str))
      (unless (eql (length parts) 2)
        (error "Malformed registration value"))
      (apply #'values parts))))

(defun send-registration-email (base username email &optional (db *data-db*))
  (with-settings (db)
    (let* ((str (encode-registration username email db))
           (url (format nil "admin/register?verify=~a"
                        (hunchentoot:url-encode str)))
           (site-name (get-setting :site-name))
           (host "localhost")
           (from "donotreply@example.com")
           (subject (format nil "~a Registration" site-name))
           (to email)
           (plist (list :base base
                        :url url
                        :site-name site-name))
           (message (fill-and-print-to-string
                     (get-style-file ".registration-email.tmpl")
                     plist)))
      (cl-smtp:send-email host from to subject message
                          :display-name site-name))))

(defun send-email-change-email (base username email db)
  (with-settings (db)
    (let* ((str (encode-registration username email db))
           (url (format nil "admin/emailchange?verify=~a"
                        (hunchentoot:url-encode str)))
           (site-name (get-setting :site-name))
           (host "localhost")
           (from "donotreply@example.com")
           (subject (format nil "~a Email Change" site-name))
           (to email)
           (plist (list :base base
                        :url url
                        :email email
                        :site-name site-name))
           (message (fill-and-print-to-string
                     (get-style-file ".email-change-email.tmpl")
                     plist)))
      (cl-smtp:send-email host from to subject message
                          :display-name site-name))))

(defun email-change (uri https verify)
  (let* ((db (get-port-db))
         (session hunchentoot:*session*)
         (uid (and session (uid-of session)))
         (user (and uid (read-user uid db))))
    (multiple-value-bind (username email)
        (ignore-errors (decode-registration verify))
      (unless (and username
                   (equal username (getf user :name))
                   (equal email (getf user :new-email)))
        (return-from email-change
          (redirect-to-error-page uri https $bad-email-change)))
      (remf user :new-email)
      (setf (getf user :mail) email
            (read-user uid db) user)
      (multiple-value-bind (base home) (compute-base-and-home uri https)
        (let ((plist (list :home home
                           :base base
                           :new-email email
                           :email-changed-p t)))
          (render-template ".profile-updated.tmpl" plist
                           :add-index-comment-links-p t
                           :data-db db))))))        

(defun profile (uri https &key verify oldpass newpass newpass2 email homepage submit)
  (let* ((db (get-port-db))
         (session hunchentoot:*session*)
         (uid (and session (uid-of session)))
         (user (and uid (read-user uid db)))
         new-user-p
         username
         errmsg
         newpass-p
         new-homepage
         new-email-p
         email-changed-p
         new-email)
    (when verify
      (when session
        (return-from profile
          (redirect-to-error-page uri https $logged-in-registration)))
      (multiple-value-setq (username email)
        (ignore-errors (decode-registration verify)))
      (unless username
        (return-from profile
          (redirect-to-error-page uri https $bad-registration)))
      (when (get-user-by-name username)
        (return-from profile
          (redirect-to-error-page uri https $used-registration)))
      (setf user (list :uid (allocate-uid db)
                       :name username
                       :mail email)
            new-user-p t))
    (unless new-user-p
      (setf username (getf user :name))
      (unless submit
        (setf email (getf user :mail)
              homepage (getf user :homepage))))
    (multiple-value-bind (base home) (compute-base-and-home uri https)
      (when submit
        (when (and new-user-p (blankp newpass))
          (setf errmsg "You must specify a password"))
        (unless (or errmsg (blankp newpass))
          (cond ((and (not new-user-p) (not (equal (md5 oldpass) (getf user :pass))))
                 (setf errmsg "Old password incorrect."))
                ((not (equal newpass newpass2))
                 (setf errmsg "New password mismatch."))
                (t (setf (getf user :pass) (md5 newpass)
                         newpass-p t))))
        (when (blankp homepage) (setf homepage nil))
        (unless (equal homepage (getf user :homepage))
          (setf (getf user :homepage) homepage
                new-homepage homepage))
        (unless (equal email (getf user :mail))
          (cond ((memq :admin (getf user :permissions))
                 (setf (getf user :mail) email
                       new-email email
                       email-changed-p t))
                (t (handler-case
                       (progn
                         (send-email-change-email base username email db)
                         (setf new-email-p t
                               new-email email
                               (getf user :new-email) email))
                     (error (c)
                       (setf errmsg
                             (format nil "Cannot send email to ~a: ~a" email c)))))))
        (cond ((and (not errmsg)
                    (or newpass-p new-homepage new-email-p email-changed-p))
               (when (or new-user-p newpass-p new-homepage
                         new-email-p email-changed-p)
                 (setf (read-user (getf user :uid) db) user)
                 (when new-user-p
                   (add-user-to-usernamehash user db)))
               (let ((plist (list :home home
                                  :base base
                                  :newpass-p newpass-p
                                  :new-homepage (efh new-homepage)
                                  :new-email (efh new-email)
                                  :new-email-p new-email-p
                                  :email-changed-p email-changed-p)))
                 (when new-user-p
                   (make-new-session (getf user :uid)))
                 (return-from profile
                   (if new-user-p
                       (render-template ".registration-complete.tmpl" plist
                                        :add-index-comment-links-p t
                                        :data-db db)
                       (render-template ".profile-updated.tmpl" plist
                                        :add-index-comment-links-p t
                                        :data-db db)))))
              (t (unless errmsg
                   (setf errmsg "No changes requested. None made.")))))
      (let ((plist (list :home home
                         :base base
                         :verify verify
                         :errmsg (efh errmsg)
                         :new-user-p new-user-p
                         :username (efh username)
                         :email (efh email)
                         :homepage (efh homepage))))
        (render-template ".profile.tmpl" plist
                         :add-index-comment-links-p t
                         :data-db db)))))

(defun register (uri https &key submit username email
                 captcha-response captcha-hidden verify)
  (when verify
    (return-from register
      (profile uri https :verify verify)))
  (multiple-value-bind (base home) (compute-base-and-home uri https)
    (let ((session hunchentoot:*session*)
          (db (get-port-db))
          errmsg
          captcha-explanation
          captcha-query
          captcha-response-size)
      (when (and session (uid-of session))
        (return-from register (profile uri https)))
      (when submit
        (cond ((blankp username)
               (setf errmsg "User name must be specified."))
              ((get-user-by-name username db)
               (setf errmsg "User name in use. Choose another."))
              ((blankp email)
               (setf errmsg "Email must be specified.")))
        (unless errmsg
          (multiple-value-bind (ok reason)
              (validate-captcha captcha-response captcha-hidden)
            (unless ok
              (setf errmsg (if (eq reason :timeout)
                               "Captcha timed out. Enter new answer."
                               "Wrong answer to captcha. Try again.")
                    captcha-response nil))))
        (unless errmsg
          (handler-case
              (send-registration-email base username email db)
            (error (c)
              (setf errmsg (format nil "Cannot send email to ~a: ~a" email c))))
          (unless errmsg
            (let ((plist (list :home home
                               :base base
                               :email (hsc email))))
              (return-from register
                (render-template ".registration-emailed.tmpl" plist
                                 :add-index-comment-links-p t
                                 :data-db db))))))
      (when (blankp captcha-response)
        (multiple-value-setq
            (captcha-explanation captcha-query
                                 captcha-response-size captcha-hidden)
          (captcha-values (make-captcha db))))
      (let ((plist (list :home home
                         :base base
                         :errmsg (hsc errmsg)
                         :username (hsc username)
                         :email (hsc email)
                         :captcha-explanation captcha-explanation
                         :captcha-query captcha-query
                         :captcha-response-size captcha-response-size
                         :captcha-response captcha-response
                         :captcha-hidden captcha-hidden)))
        (render-template ".register.tmpl" plist
                         :add-index-comment-links-p t
                         :data-db db)))))
        

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
    (render-template ".error.tmpl" plist
                     :add-index-comment-links-p t
                     :data-db db)))

;; <baseurl>/admin/?node=<node-num>
(defun render-web-node (node-num uri https &key alias (data-db (get-port-db)))
  (unless (setf node-num (ignore-errors (parse-integer node-num)))
    (return-from render-web-node (not-found)))
  (with-settings (data-db)
    (let* ((plist (make-node-plist node-num :unpublished-p t :data-db data-db))
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
        (render-template post-template-name plist
                         :add-index-comment-links-p t
                         :data-db data-db)))))

(defun redirect-uri (uri https)
  (if uri
      (format nil "~a://~a" (if https "https" "http") uri)
      "http://google.com/search?&q=loser"))

(defun node-format-to-edit-post-plist (format)
  (cond ((eql format $filtered-html-format) '(:filtered-html t))
        ((eql format $full-html-format) '(:full-html t))
        ((or (eql format $raw-html-format)
             (eql format $old-raw-html-format))
         '(:raw-html t))))

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

(defparameter *edit-post-category-options-per-row* 4)

(defun node-to-edit-post-category-rows (node &optional (db *data-db*))
  (flet ((ncdrs (list)
           (nthcdr *edit-post-category-options-per-row* list)))
    (let ((options (node-to-edit-post-category-options node db)))
      (loop for tail on options by #'ncdrs
         collect
           (list
            :category-options
            (subseq
             tail 0
             (min (length tail) *edit-post-category-options-per-row*)))))))

(defun update-node-categories (node categories old-categories &key
                               (data-db *data-db*)
                               (site-db *site-db*))
  (when (integerp node)
    (setf node (read-node node data-db)))
  (let ((nid (getf node :nid))
        (created (getf node :created))
        (neighbors (getf node :cat-neighbors))
        (added (set-difference categories old-categories))
        (removed (set-difference old-categories categories)))
    (dolist (cat removed)
      (let* ((prev.next (getf neighbors cat))
             (prev-nid (car prev.next))
             (next-nid (cdr prev.next)))
        (unless (or (eql prev-nid nid) (eql next-nid nid))
          (let* ((prev (read-node prev-nid data-db))
                 (prev.next (getf (getf prev :cat-neighbors) cat)))
            (when (consp prev.next)
              (setf (cdr prev.next) next-nid
                    (read-node prev-nid data-db) prev)
              (render-node prev :data-db data-db :site-db site-db))
            (let* ((next (read-node next-nid data-db))
                   (prev.next (getf (getf next :cat-neighbors) cat)))
              (when (consp prev.next)
                (setf (car prev.next) prev-nid
                      (read-node next-nid data-db) next)
                (render-node next :data-db data-db :site-db site-db))))))
      (remf neighbors cat)
      (setf (getf node :cat-neighbors) neighbors)
      (let ((catnodes (delete nid (read-catnodes cat data-db) :key #'car)))
        (setf (read-catnodes cat data-db) catnodes)))
    (dolist (cat added)
      (let ((catnodes (delete nid (read-catnodes cat data-db) :key #'car)))
        (setf catnodes (merge 'list `((,nid . ,created)) catnodes #'> :key #'cdr))
        (setf (read-catnodes cat data-db) catnodes)
        (loop for tail on catnodes
           for next-cell = nil then cell
           for cell = (car tail)
           when (eql nid (car cell)) do
             (unless next-cell (setf next-cell (car (last tail))))
             (let* ((prev-cell (if (cdr tail) (cadr tail) (car catnodes)))
                    (prev-nid (car prev-cell))
                    (next-nid (car next-cell)))
               (setf (getf neighbors cat) (cons prev-nid next-nid))
               (let* ((prev (read-node prev-nid data-db))
                      (prev.next (getf (getf prev :cat-neighbors) cat)))
                 (when (consp prev.next)
                   (setf (cdr prev.next) nid
                         (read-node prev-nid data-db) prev)
                   (render-node prev :data-db data-db :site-db site-db)))
               (let* ((next (read-node next-nid data-db))
                      (prev.next (getf (getf next :cat-neighbors) cat)))
                 (when (consp prev.next)
                   (setf (car prev.next) nid
                         (read-node next-nid data-db) next)
                   (render-node next :data-db data-db :site-db site-db)))))))
    (setf (getf node :cat-neighbors) neighbors)
    node))

;; <baseurl>/admin/add_post
(defun add-post (uri https)
  (edit-post nil uri https))

;; <baseurl>/admin/?edit_post=<node-num>
(defun edit-post (node-num uri https)
  (let* ((session hunchentoot:*session*)
         (db (get-port-db))
         (uid (uid-of session))
         (user (read-user uid db))
         (permissions (getf user :permissions))
         (author (getf user :name))
         (node (and node-num (read-node node-num db)))
         (alias (car (getf node :aliases)))
         (title (getf node :title))
         (created (getf node :created))
         (promote (if node (getf node :promote) 1))
         (status (if node (getf node :status) 1))
         (body (getf node :body))
         (format (if node (getf node :format) 1))
         plist)
    (when (and node-num (not node))
      (return-from edit-post
        (redirect-to-error-page uri https $unknown-post-number)))
    (unless (or (memq :admin permissions)
                (memq :poster permissions))
      (return-from edit-post
        (redirect-to-error-page uri https $no-add-or-edit-permission)))
    (unless node
      (let ((category (with-settings (db)
                        (get-setting :default-category))))
        (when category
          (setf (getf node :cat-neighbors) `(,category (0 . 0))))))
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
                         :category-rows (node-to-edit-post-category-rows node db)
                         (node-format-to-edit-post-plist format))))
    (render-template ".edit-post.tmpl" plist
                     :add-index-comment-links-p t
                     :data-db db)))

(defvar *node-save-lock*
  (bt:make-recursive-lock "*node-save-lock*"))

(defmacro with-node-save-lock (&body body)
  `(bt:with-recursive-lock-held (*node-save-lock*)
     ,@body))

(defun compute-alias-from-title (title)
  (let ((alias (string-downcase
                (fsdb:str-replace
                 " " "_"
                 (delete-if-not (lambda (ch)
                                  (or (digit-char-p ch)
                                      (alpha-char-p ch)
                                      (member ch '(#\space #\_))))
                                title))))
        new-alias)
    (loop
       (setf new-alias (fsdb:str-replace "__" "_" alias))
       (when (equal new-alias alias) (return))
       (setf alias new-alias))
    (strcat alias ".html")))

(defun compute-new-alias (node title alias &key (data-db *data-db*) (site-db *site-db*))
  (when (integerp node)
    (setf node (read-node node data-db)))
  (when (blankp title)
    (setf title (getf node :title)))
  (when (blankp alias)
    (setf alias (compute-alias-from-title title)))
  (let ((len (length alias))
        (html-p t)
        (aliases (getf node :aliases)))
    (unless (or (member alias aliases :test #'equal)
                (not (fsdb:db-get site-db alias)))
      (when (eql (search ".html" alias :from-end t :test #'equal)
                 (- len 5))
        (setf alias (subseq alias 0 (- len 5))))
      ;; Support Windows folks who like .htm
      (when (eql (search ".htm" alias :from-end t :test #'equal)
                 (- len 4))
        (setf alias (subseq alias 0 (- len 4))
              html-p nil))
      (loop for i from 2
         for new-alias = (format nil "~a_~d.~a" alias i (if html-p "html" "htm"))
         unless (fsdb:db-probe site-db new-alias)
         do
           (setf alias new-alias)
           (return))))
  alias)

;; Remove all html pages for NODE, except index.html, which needs to be
;; regenerated after calling this.
;; Remove NODE from its year and month pages.
;; Unsplice NODE from the :cat-neighbors lists in its neighbors
;; Don't need to remove node from its catnodes files.
(defun remove-node-from-site (node &key (data-db *data-db*) (site-db *site-db*))
  (when (integerp node)
    (setf node (read-node node data-db)))
  (dolist (alias (getf node :aliases))
    (setf (fsdb:db-get site-db alias) nil))
  (update-node-year-and-month-pages node :data-db data-db :site-db site-db)
  (loop with nid = (getf node :nid)
     with cat-neighbors = (getf node :cat-neighbors)
     for (cat (prev . next)) on cat-neighbors by #'cddr
     do
       (setf (read-catnodes cat data-db)
             (delete nid (read-catnodes cat data-db)))
       (unless (or (eql prev nid) (eql next nid))
         (let* ((prev-node (read-node prev data-db))
                (prev.next (getf (getf prev-node :cat-neighbors) cat)))
           (when (and prev.next (eql (cdr prev.next) nid))
             (setf (cdr prev.next) next
                   (read-node prev data-db) prev-node)
             (render-node prev-node :data-db data-db :site-db site-db)))
         (let* ((next-node (read-node next data-db))
                (prev.next (getf (getf next-node :cat-neighbors) cat)))
           (when (and prev.next (eql (car prev.next) nid))
             (setf (car prev.next) prev
                   (read-node next data-db) next-node)
             (render-node next-node :data-db data-db :site-db site-db))))))

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
          (delete-aliases-p nil)
          (nid (getf node :nid))
          new-alias)
      (setf new-alias (compute-new-alias node title alias
                                         :data-db data-db
                                         :site-db site-db))
      (when (blankp created)
        (setf created now))
      (cond (new-p
             (setf node (list :aliases (list new-alias)
                              :nid (setf nid (allocate-nid data-db))
                              :title title
                              :uid uid
                              :status status
                              :created created
                              :changed now
                              :promote promote
                              :body body
                              :format format)
                   alias new-alias
                   new-alias-p t)
             ;; So that the node exists when update-node-categories is called
             (setf (read-node nid data-db) node))
            (t (setf (getf node :changed) now)
               (let ((aliases (getf node :aliases)))
                 (unless (string= new-alias (car aliases))
                   (when alias
                     (setf aliases (delete alias aliases :test #'string=)))
                   (setf aliases (delete new-alias aliases :test #'string=))
                   (push new-alias aliases)
                   (setf (getf node :aliases) aliases)
                   (setf new-alias-p t)))
               (unless (string= title (getf node :title))
                 (setf (getf node :title) title
                       new-alias-p t))
               (unless (eql status (getf node :status))
                 (setf (getf node :status) status)
                 (cond ((eql status 1) (setf new-alias-p t))
                       (t (setf delete-aliases-p t))))
               (setf (getf node :changed) now
                     (getf node :promote) promote
                     (getf node :body) body
                     (getf node :format) format)))
      (let ((old-categories
             (loop for (cat) on (getf node :cat-neighbors) by #'cddr
                collect cat)))
        (unless (and (eql (length old-categories)
                          (length categories))
                     (eql (length old-categories)
                          (length (union categories old-categories))))
          (setf node (update-node-categories
                      node categories old-categories
                      :data-db data-db
                      :site-db site-db))))
      (setf (read-node nid data-db) node)
      (cond (delete-aliases-p
             (remove-node-from-site node :data-db data-db :site-db site-db)
             (setf alias (format nil "admin/?node=~d" nid)))
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
         (site-db (with-site-db (db) *site-db*))
         (uid (uid-of session))
         (user (read-user uid db))
         (author (getf user :name))
         (permissions (getf user :permissions))
         (node (and node-num (read-node node-num db)))
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
                          :category-rows (node-to-edit-post-category-rows
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
           (render-template ".edit-post.tmpl" plist
                            :add-index-comment-links-p t
                            :data-db db))
          (submit
           (setf alias
                 (save-updated-node node
                                    :data-db db
                                    :site-db site-db
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
          (delete
           (when (blankp node-num)
             (return-from submit-post
               (redirect-to-error-page uri https $cant-delete)))
           (setf (getf node :status) 0) ;causes it to disappear from year & month pages
           (remove-node-from-site node :data-db db :site-db site-db)
           (setf (read-node node-num db) nil)
           (render-site-index :data-db db :site-db site-db)
           (let ((base (compute-base-and-home uri https)))
             (hunchentoot:redirect base))))))

;; <baseurl>/admin/?add_comment=<node-num>
(defun add-comment (node-num uri https)
  (edit-comment nil uri https :node-num node-num))

;; <baseurl>/admin/?edit_comment=<comment-num>
(defun edit-comment (comment-num uri https &key node-num)
  (let* ((session hunchentoot:*session*)
         (db (get-port-db))
         (session-uid (and session (uid-of session)))
         (user (and session-uid (read-user session-uid db)))
         (permissions (getf user :permissions))
         (admin-p (memq :admin permissions))
         (comment (and comment-num (read-comment comment-num db)))
         (node (and (or node-num (setf node-num (getf comment :nid)))
                    (read-node node-num db)))
         (post-alias (efh (car (getf node :aliases))))
         (post-title (efh (getf node :title)))
         (uid (getf comment :uid))
         (title (getf comment :subject))
         (body (getf comment :comment))
         (created (or (getf comment :timestamp) (get-unix-time)))
         (format (getf comment :format))
         (author (getf comment :name))
         (email (getf comment :mail))
         (homepage (getf comment :homepage))
         (status (getf comment :status))
         captcha-explanation captcha-query captcha-response-size captcha-hidden)
    (when (and (blankp author) user)
      (setf author (getf user :name)))
    (when (and user (not comment-num))
      (setf email (getf user :mail)
            homepage (getf user :homepage)
            status 0
            format 1))      
    (unless (or (null comment-num)
                (and session-uid user
                     (or admin-p (eql uid session-uid))))
      (return-from edit-comment
        (redirect-to-error-page uri https $no-edit-comment-permission)))
    (unless user
      (let ((captcha (make-captcha db)))
        (setf captcha-explanation (captcha-query-explanation captcha)
              captcha-query (captcha-query-html captcha)
              captcha-response-size (captcha-response-size captcha)
              captcha-hidden (captcha-hidden-value captcha))))
    (multiple-value-bind (base home) (compute-base-and-home uri https)
      (let ((plist (list* :comment-num comment-num
                          :node-num node-num
                          :post-alias post-alias
                          :post-title post-title
                          :moderated-p (null user)
                          :home home
                          :base base
                          :author (efh author)
                          :email (efh email)
                          :homepage (efh homepage)
                          :post-time (unix-time-to-rfc-1123-string created)
                          :title (efh title)
                          :published (eql status 0)
                          :body (efh body)
                          :edit-name-p (not uid)
                          :show-published-p admin-p
                          :show-input-format admin-p
                          :captcha-explanation captcha-explanation
                          :captcha-query captcha-query
                          :captcha-response-size captcha-response-size
                          :captcha-hidden captcha-hidden
                          (node-format-to-edit-post-plist format))))
        (render-template ".edit-comment.tmpl" plist
                         :add-index-comment-links-p t
                         :data-db db)))))

(defun save-updated-comment (comment &key
                             (data-db *data-db*)
                             (site-db *site-db*)
                             nid
                             uid
                             subject
                             body
                             (status 0)
                             (format $filtered-html-format)
                             name
                             mail
                             homepage)
  (check-type subject string)
  (check-type uid (or null integer))
  (check-type body string)
  (check-type status integer)
  (assert (member format *valid-post-format-values*))
  (assert (and (not (blankp subject)) (not (blankp body))))
  (unless nid (setf nid (getf comment :nid)))
  (with-node-save-lock
    (let ((new-p (null comment))
          (timestamp (or (getf comment :timestamp) (get-unix-time)))
          (node (read-node nid data-db))
          (cid (getf comment :cid)))
      (assert node)
      (cond (new-p
             (setf comment (list :cid (setf cid (allocate-cid data-db))
                                 :nid nid
                                 :uid uid
                                 :subject subject
                                 :comment body
                                 :timestamp timestamp
                                 :status status
                                 :format format
                                 :name name
                                 :mail mail
                                 :homepage homepage))
             (push cid (getf node :comments))
             (setf (read-node nid data-db) node))
            (t (setf (getf comment :subject) subject
                     (getf comment :status) status
                     (getf comment :comment) body
                     (getf comment :format) format
                     (getf comment :homepage) homepage)))
      (setf (read-comment cid data-db) comment)
      (cond ((eql 0 status)
             (let ((numbers (unmoderated-comment-numbers data-db)))
               (when (member cid numbers)
                 (setf (unmoderated-comment-numbers data-db)
                       (delete cid numbers)))))
            (t (pushnew cid (unmoderated-comment-numbers data-db))))
      (render-node node :data-db data-db :site-db site-db)
      ;; Don't always have to do this, but figuring out when
      ;; is harder than just doing it.
      (render-site-index :data-db data-db :site-db site-db)
      (values cid (car (getf node :aliases))))))

(defun submit-comment (uri https &key comment-num node-num author email homepage
                       title published body input-format
                       captcha-response captcha-hidden
                       preview submit delete)
  (let* ((session hunchentoot:*session*)
         (db (get-port-db))
         (site-db (with-site-db (db) *site-db*))
         (session-uid (and session (uid-of session)))
         (user (and session-uid (read-user session-uid db)))
         (permissions (getf user :permissions))
         (admin-p (memq :admin permissions))
         (comment (and comment-num (read-comment comment-num db)))
         (node (and node-num (read-node node-num db)))
         (post-alias (efh (car (getf node :aliases))))
         (post-title (efh (getf node :title)))
         (uid (getf comment :uid))
         (created (getf comment :timestamp))
         (post-time (unix-time-to-rfc-1123-string
                     (or created (get-unix-time))))
         (status (if session-uid
                     (if (blankp published) 1 0)
                     1))                ;not published for unlogged-in commentor
         (format (if admin-p
                     (node-format-name-to-number input-format)
                     1))                ;filtered-html for non-admin commentor
         (errmsg nil)
         captcha-explanation captcha-query captcha-response-size
         plist)
    (when comment
      (when (blankp author) (setf author (getf comment :name)))
      (when (blankp email) (setf email (getf comment :mail))))
    (cond ((blankp author)
           (setf errmsg "Name may not be blank"))
          ((blankp body)
           (setf errmsg "Comment may not be blank")
           (when comment
             (setf body (getf comment :comment)))))
    (when (blankp title)
      (let* ((body-len (length body))
             (pos (if (<= body-len 20)
                      body-len
                      (or (position #\space body :start 20) 20))))
        (setf title (subseq body 0 pos))))
    (unless (or (null comment-num)
                (and session-uid user
                     (or admin-p (eql uid session-uid))))
      (return-from submit-comment
        (redirect-to-error-page uri https $no-edit-comment-permission)))
    (when (and comment-num (not comment))
      (return-from submit-comment
        (redirect-to-error-page uri https $unknown-comment-number)))
    (when (blankp email) (setf email nil))
    (when (blankp homepage) (setf homepage nil))
    (unless user
      (unless (or errmsg (and preview (blankp captcha-response)))
        (multiple-value-bind (ok reason)
            (validate-captcha captcha-response captcha-hidden)
          (unless ok
            (setf errmsg
                  (if (eq reason :timeout)
                      "Captcha timed out. Enter new answer."
                      "Wrong answer to captcha. Try again."))
            (setf captcha-response nil))))
      (when (blankp captcha-response)
        (let ((captcha (make-captcha db)))
          (setf captcha-explanation (captcha-query-explanation captcha)
                captcha-query (captcha-query-html captcha)
                captcha-response-size (captcha-response-size captcha)
                captcha-hidden (captcha-hidden-value captcha)))))
    (cond ((or preview errmsg)
           (multiple-value-bind (base home) (compute-base-and-home uri https)
             (setf plist
                   (list* :comment-num comment-num
                          :node-num node-num
                          :post-alias post-alias
                          :post-title post-title
                          :moderated-p (null user)
                          :home home
                          :base base
                          :errmsg (efh errmsg)
                          :author (efh author)
                          :email (efh email)
                          :homepage (efh homepage)
                          :post-time post-time
                          :title (efh title)
                          :published (eql status 0)
                          :body (efh body)
                          :edit-name-p (not uid)
                          :show-published-p admin-p
                          :show-input-format admin-p
                          :captcha-explanation captcha-explanation
                          :captcha-query captcha-query
                          :captcha-response-size captcha-response-size
                          :captcha-response captcha-response
                          :captcha-hidden captcha-hidden
                          (node-format-to-edit-post-plist format)))
             (let* ((template-name (get-comment-template-name db))
                    (template (get-style-file template-name db))
                    (node (and node-num (read-node node-num db)))
                    (alias (car (getf node :aliases)))
                    (comment-plist
                     (list :cid comment-num
                           :subject (efh title)
                           :name (efh author)
                           :homepage (efh homepage)
                           :post-date post-time
                           :comment (drupal-format body format)
                           :home home
                           :permalink (and alias (efh alias)))))
               (setf (getf plist :preview)
                     (fill-and-print-to-string template comment-plist))
               (render-template ".edit-comment.tmpl" plist
                                :add-index-comment-links-p t
                                :data-db db))))
          (submit
           (multiple-value-bind (cid alias)
               (save-updated-comment comment
                                     :data-db db
                                     :site-db site-db
                                     :nid node-num
                                     :uid uid
                                     :subject title
                                     :body body
                                     :status status
                                     :format format
                                     :name author
                                     :mail email
                                     :homepage homepage)
             (multiple-value-bind (base home) (compute-base-and-home uri https)
               (cond ((eql 0 status)
                      (hunchentoot:redirect
                       (format nil "~a~a#comment-~a" base alias cid)))
                     (t (let ((plist (list :base base
                                           :home home
                                           :alias alias)))
                          (render-template ".comment-submitted.tmpl" plist
                                           :add-index-comment-links-p t
                                           :data-db db)))))))
          (delete
           (when (blankp comment-num)
             (return-from submit-comment
               (redirect-to-error-page uri https $cant-delete)))
           (let* ((nid (getf comment :nid))
                  (node (and nid (read-node nid db))))
             (setf (read-comment comment-num db) nil)
             (when node
               (setf (getf node :comments) (delete comment-num (getf node :comments))
                     (read-node nid db) node)
               (render-node node :data-db db :site-db site-db))
             (render-site-index :data-db db :site-db site-db)
             (let ((base (compute-base-and-home uri https)))
               (cond ((eql 0 (getf comment :status))
                      (let ((alias (or (car (getf node :aliases)) "")))
                        (unless (blankp alias)
                          (setf alias (strcat alias "#comments")))
                        (hunchentoot:redirect (format nil "~a~a" base alias))))
                     (t (hunchentoot:redirect
                         (format nil "~aadmin/moderate" base))))))))))

;; <baseurl>/admin/settings
(defun settings (uri https)
  (let* ((db (get-port-db))
         (session hunchentoot:*session*)
         (uid (uid-of session))
         (user (read-user uid db))
         (admin-p (memq :admin (getf user :permissions)))
         plist)
    (multiple-value-bind (base home) (compute-base-and-home uri https)
      (cond ((hunchentoot:parameter "logout")
             (end-session)
             (return-from settings (hunchentoot:redirect base))))
      (setf plist (list :home home
                        :base base
                        :admin-p admin-p))
      (render-template ".settings.tmpl" plist
                       :add-index-comment-links-p t
                       :data-db db))))

(defun threads (uri https)
  (let ((threads (bt:all-threads)))
    (setf threads
          #+ccl (sort threads #'> :key #'ccl:process-serial-number)
          #-ccl (sort threads #'string-lessp :key #'bt:thread-name))
    (loop for tail on threads
       for thread = (car tail)
       for serial = #+ccl (ccl:process-serial-number thread) #-ccl nil
       for name = (bt:thread-name thread)
       for whostate = #+ccl (ccl:process-whostate thread) #-ccl nil
       do (setf (car tail) (list :index serial :name name :status whostate)))
    (multiple-value-bind (base home) (compute-base-and-home uri https)
      (let ((plist (list :home home
                         :base base
                         :threads threads
                         :room (with-output-to-string (*standard-output*)
                                 (room t))
                         :index-p #+ccl t #-ccl nil
                         :status-p #+ccl t #-ccl nil)))
        (render-template ".threads.tmpl" plist
                         :add-index-comment-links-p t
                         :data-db (get-port-db))))))

;; <baseurl>/admin/moderate
(defun moderate (uri https)
  (submit-moderate uri https))

(defun delete-comment (cid &key (data-db *data-db*) (site-db *site-db*)
                       (render-index-p t))
  (let ((comment (read-comment cid data-db)))
    (when comment
      (let* ((nid (getf comment :nid))
             (node (read-node nid data-db)))
        (when node
          (setf (getf node :comments) (delete cid (getf node :comments))
                (read-node nid data-db) node)
          (when (eql 0 (getf comment :status))
            (render-node node :data-db data-db :site-db site-db))
          (when render-index-p
            (render-site-index :data-db data-db :site-db site-db)))
        (setf (read-comment cid) nil)
        (setf (unmoderated-comment-numbers data-db)
              (delete cid (unmoderated-comment-numbers data-db)))))))

(defun approve-comment (cid &key (data-db *data-db*) (site-db *site-db*)
                        (render-index-p t))
  (let ((comment (read-comment cid data-db)))
    (when (and comment (not (eql 0 (getf comment :status))))
      (setf (getf comment :status) 0
            (read-comment cid data-db) comment)      
      (let* ((nid (getf comment :nid))
             (node (read-node nid data-db)))
        (when node
          (render-node node :data-db data-db :site-db site-db)
          (when render-index-p
            (render-site-index :data-db data-db :site-db site-db)))))
    (setf (unmoderated-comment-numbers data-db)
          (delete cid (unmoderated-comment-numbers data-db)))))

(defun submit-moderate (uri https &key submit)
  (let* ((db (get-port-db))
         (site-db (with-site-db (db) *site-db*))
         (session hunchentoot:*session*)
         (uid (uid-of session))
         (user (read-user uid db))
         (admin-p (memq :admin (getf user :permissions)))
         comment-numbers comments plist)
    (unless admin-p
      (return-from submit-moderate
        (redirect-to-error-page uri https $no-moderation-permission)))
    (when submit
      (dolist (cell (hunchentoot:post-parameters hunchentoot:*request*))
        (let ((name (car cell))
              (value (cdr cell)))
          (when (eql 0 (search "c-" name :test #'equal))
            (let ((cid (ignore-errors (parse-integer (subseq name 2)))))
              (when cid
                (cond ((equal value "x")
                       (delete-comment cid :data-db db :site-db site-db
                                       :render-index-p nil))
                      ((equal value "ok")
                       (approve-comment cid :data-db db :site-db site-db
                                        :render-index-p nil))))))))
      (render-site-index :data-db db :site-db site-db))
    (setf comment-numbers (sort (unmoderated-comment-numbers db) #'>))
    (loop with cnt = 100
       for cid in comment-numbers
       for comment = (read-comment cid db)
       for name = (efh (getf comment :name))
       for email = (efh (getf comment :mail))
       for homepage = (efh (getf comment :homepage))
       for subject = (efh (getf comment :subject))
       for radio-name = (format nil "c-~d" cid)
       for ok-checked = (equal "ok" (hunchentoot:post-parameter radio-name))
       when comment do
         (push (list :name name
                     :email (unless (blankp email) email)
                     :homepage (unless (blankp homepage) homepage)
                     :cid cid
                     :subject (if (blankp subject) "[blank]" subject)
                     :radio-name radio-name
                     :x-checked (not ok-checked)
                     :ok-checked ok-checked)
               comments)
         (when (<= (decf cnt) 0) (return))
       unless comment do
         (setf (unmoderated-comment-numbers db)
               (delete cid (unmoderated-comment-numbers db))))
    (setf comments (nreverse comments))
    (multiple-value-bind (base home) (compute-base-and-home uri https)
      (setf plist (list :home home
                        :base base
                        :comments comments))
      (render-template ".moderate-comments.tmpl" plist
                       :add-index-comment-links-p t
                       :data-db db))))


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
