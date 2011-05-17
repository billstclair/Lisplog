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

(defparameter *style-index-file* ".index.tmpl")
(defparameter *style-post-file* ".post.tmpl")

;;;
;;; Accessing styles and site files
;;;

(defun get-style-file (file &optional (db *data-db*))
  (with-settings (db)
    (let ((style (get-setting :style)))
      (or (fsdb:db-get *styles-db* style file)
          (fsdb:db-get *styles-db* $DEFAULT file)
          (error "No index template for style: ~s" style)))))

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
  (funcall thunk (or *settings* (read-settings data-db))))

;;;
;;; Template operations
;;;

(defun data-get (dir file &key (db *data-db*) (subdirs-p t))
  (node-get db dir file :subdirs-p subdirs-p))

(defun (setf data-get) (value dir file &key (db *data-db*) (subdirs-p t))
  (setf (node-get db dir file :subdirs-p subdirs-p) value))

(defun read-node (node &optional (db *data-db*))
  (node-get db $NODES node))

(defun (setf read-node) (plist node &optional (db *data-db*))
  (setf (node-get db $NODES node) plist))

(defun read-comment (comment &optional (db *data-db*))
  (node-get db $COMMENTS comment))

(defun (setf read-comment) (plist comment &optional (db *data-db*))
  (setf (node-get db $COMMENTS comment) plist))

(defun read-user (user-num &optional (db *data-db*))
  (node-get db $USERS user-num))

(defun (setf read-user) (plist user-num &optional (db *data-db*))
  (setf (node-get db $USERS user-num) plist))

(defun user-permission-p (user-num permission &optional (db *data-db*))
  (not (null (memq permission (getf (read-user user-num db) :permissions)))))

(defun (setf user-permission-p) (value user-num permission &optional (db *data-db*))
  (let ((user (read-user user-num db)))
    (unless user (error "No user number: ~s" user-num))
    (if value
        (pushnew permission (getf user :permissions))
        (setf (getf user :permissions)
              (delq permission (getf user :permissions))))
    (setf (read-user user-num db) user)
    value))

(defun read-catnodes (cat &optional (db *data-db*))
  (node-get db $CATNODES cat :subdirs-p nil))

(defun (setf read-catnodes) (value cat &optional (db *data-db*))
  (setf (node-get db $CATNODES can :subdirs-p nil) value))

(defun read-category (cat &optional (db *data-db*))
  (node-get db $CATEGORIES cat :subdirs-p nil))

(defun fill-and-print-to-string (template values)
  (with-output-to-string (stream)
    (let ((template:*string-modifier* 'identity))
      (template:fill-and-print-template template values :stream stream))))

(defun unix-time-to-rfc-1123-string (&optional unix-time)
  (hunchentoot:rfc-1123-date
   (if unix-time
       (unix-to-universal-time unix-time)
       (get-universal-time))))

(defparameter *short-month-names*
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;; "Sat, 07 May 2011 12:53:33 GMT"
(defun parse-rfc-1123-date (string)
  (let ((parts (split-sequence:split-sequence #\space string)))
    (assert
     (and (eql (length parts) 6)
          (string-equal "GMT" (elt parts 5))))
    (let ((day (parse-integer (elt parts 1)))
          (month (1+ (position (elt parts 2) *short-month-names*
                               :test #'string-equal)))
          (year (parse-integer (elt parts 3)))
          (time-parts (split-sequence:split-sequence #\: (elt parts 4))))
      (assert (eql 3 (length time-parts)))
      (let ((h (parse-integer (elt time-parts 0)))
            (m (parse-integer (elt time-parts 1)))
            (s (parse-integer (elt time-parts 2))))
        (encode-universal-time s m h day month year 0)))))

(defun rfc-1123-string-to-unix-time (string)
  (universal-to-unix-time (parse-rfc-1123-date string)))

(defun efh (string)
  (and string (hunchentoot:escape-for-html string)))

(defun do-drupal-quotes (str)
  (fsdb:str-replace
   "[quote]" "</p><blockquote><p>"
   (fsdb:str-replace "[/quote]" "</p></blockquote><p>" str)))

(defun do-drupal-line-breaks (str)
  (with-input-from-string (s str)
    (with-output-to-string (os)
      (princ "<p>" os)
      (do-drupal-line-breaks-internal s os)
      (princ "</p>" os))))

(defun do-drupal-line-breaks-internal (s os)
  (loop with last-ch-newline-p = nil
     for ch = (read-char s nil :eof)
     until (eq ch :eof)
     do
       (cond ((eql ch #\newline)
              (cond (last-ch-newline-p
                     (format os "</p>~%~%<p>")
                     (setf last-ch-newline-p nil))
                    (t (setf last-ch-newline-p t))))
             (t (when last-ch-newline-p
                  (format os "<br/>~&")
                  (setf last-ch-newline-p nil))
                (write-char ch os)))))

(defun eliminate-empty-paragraphs (str)
  (fsdb:str-replace "<p></p>" "" str))

(defun process-interwiki-references (str)
  (let ((matches nil))
    (cl-ppcre:do-scans (ms me rs re
                           "\\[(.+?)\\:(.*?)\\]"
                           str)
      (push (cons rs re) matches))
    (loop for (rs . re) in matches
       for ks = (aref rs 0)
       for ke = (aref re 0)
       for key = (subseq str ks ke)
       for url = (getf (data-get $INTERWIKI key :subdirs-p nil) :iw_url)
       for ns = (aref rs 1)
       for ne = (aref re 1)
       for name = (if (eql ns ne) key (subseq str ns ne))
       when url
       do
         (setf str (strcat (subseq str 0 ks)
                           "<a href='"
                           url
                           "'>"
                           name
                           "</a>"
                           (subseq str ne)))))
  str)

(defun drupal-format (str)
  (process-interwiki-references
   (eliminate-empty-paragraphs
    (do-drupal-line-breaks
        (do-drupal-quotes str)))))

;; This deals with [quote]...[/quote] from Drupal
(defun do-drupal-formatting (plist)
  (let ((body (getf plist :body))
        (teaser (getf plist :teaser)))
      (when body
        (setf (getf plist :body) (drupal-format body)))
      (when teaser
        (setf (getf plist :teaser) (drupal-format teaser))))
  plist)

(defun fill-templates-in-plist (plist values)
  (let ((res (copy-list plist)))
    (loop for tail on (cdr res) by #'cddr
       for val = (car tail)
       when (stringp val)
       do
         (setf (car tail) (fill-and-print-to-string val values)))
    res))

;; May eventually use more than the HTML property of each block
(defun get-blocks (&optional (settings *settings*))
  (loop for block-num in (getf settings :block-nums)
     collect (data-get $BLOCKS block-num)))

(defun fetch-comments (numbers &optional (*data-db* *data-db*))
  (unless (listp numbers)
    (let ((plist (data-get $NODES numbers)))
      (setf numbers (getf plist :comments))))
  (setf numbers (sort (copy-list numbers) #'<))
  (loop for num in numbers
     for plist = (data-get $COMMENTS num)
     for text = (getf plist :comment)
     for unapproved-p = (eql 1 (getf plist :status))
     unless unapproved-p
     do
       (setf (getf plist :comment) (drupal-format text)
             (getf plist :post-date)
             (unix-time-to-rfc-1123-string (getf plist :timestamp)))
       (when (blankp (getf plist :homepage))
         (setf (getf plist :homepage) nil))
     unless unapproved-p
     collect plist))

(defun render-template (template-name plist &key
                        month year
                        (data-db *data-db*) index-template-name)
  (with-settings ()
    (let* ((template (get-style-file template-name data-db))
           (index-template
            (get-style-file (or index-template-name *style-index-file*) data-db)))
      (unless (getf plist :home) (setf (getf plist :home) "."))
      (setf plist
            `(:blocks ,(get-blocks)
              :page-content ,(fill-and-print-to-string template plist)
               ,@(compute-months-and-years-link-plist month year data-db)
               ,@plist
               ,@*settings*))
      (let* ((res (fill-and-print-to-string index-template plist)))
        (loop for new-res = (fill-and-print-to-string res plist)
           until (equal res new-res)
           do (setf res new-res))
        res))))

(defun determine-home (alias)
  (let ((count
         (length
          (cdr (pathname-directory (merge-pathnames alias "/" nil))))))
    (cond ((eql count 0) ".")
          (t (let ((res ".."))
               (dotimes (i (1- count))
                 (setf res (strcat "../" res)))
               res)))))

(defun cat-neighbors-rendering-plist (node-plist &optional (db *data-db*))
  (when (atom node-plist)
    (setf node-plist (read-node node-plist db)))
  (loop with neighbors = (getf node-plist :cat-neighbors)
     for nid = (getf node-plist :nid)
     for (cat (prev . next)) on neighbors by #'cddr
     for cat-plist = (read-category cat db)
     for cat-name = (getf cat-plist :name)
     for cat-desc = (or (getf cat-plist :description) cat-name)
     for prev-url = (unless (eql prev nid)
                      (car (getf (read-node prev db) :aliases)))
     for next-url = (unless (eql next nid)
                      (if (eql prev next)
                          prev-url
                          (car (getf (read-node next db) :aliases))))
     when cat-name collect
       `(:cat-name ,cat-name
         :cat-desc ,cat-desc
         :prev-url ,prev-url
         :next-url ,next-url)))

(defconstant $filtered-html-format 1)
(defconstant $full-html-format 3)
(defconstant $raw-html-format 5)

(defparameter *valid-post-format-values*
  (list $filtered-html-format $full-html-format $raw-html-format))

(defun make-node-plist (node &key (comments-p t) (data-db *data-db*))
  (with-settings ()
    (let* ((plist (or (if (listp node) node (read-node node data-db))
                      (error "Node does not exist: ~s" node)))
           (created (getf plist :created))
           (status (getf plist :status))
           (uid (getf plist :uid))
           (format (getf plist :format))
           (user-plist (data-get $USERS uid :db data-db)))
      (when (eql status 1)
        (unless (eql format $raw-html-format)
          (setf plist (do-drupal-formatting plist)))
        (setf (getf plist :post-date)
              (unix-time-to-rfc-1123-string created))
        (setf (getf plist :author) (getf user-plist :name))
        (setf (getf plist :category-info)
              (cat-neighbors-rendering-plist plist data-db))
        (when comments-p
          (let ((comment-plists (fetch-comments (getf plist :comments) data-db)))
            (setf (getf plist :comment-plists) comment-plists
                  ;; Note :count-comments here and :comment-count below
                  (getf plist :count-comments) (length comment-plists))))
        plist))))

(defun get-post-template-name (&optional (db *data-db*))
  (with-settings (db)
    (or (get-setting :post-template) *style-post-file*)))

(defun render-node (node &key (data-db *data-db*) (site-db *site-db*))
  (with-settings ()
    (let* ((plist (make-node-plist node :data-db data-db))
           (aliases (getf plist :aliases))
           (post-template-name (get-post-template-name data-db)))
      (when plist
        (setf plist `(:posts
                      (,plist)
                      :page-title ,(getf plist :title)
                      ,@(compute-history-plist node data-db)))
        (dolist (alias aliases)
          ;; This needs to change based on the path in each alias
          (setf (getf plist :home) (determine-home alias))
          (setf (getf plist :permalink) alias)
          (setf (fsdb:db-get site-db alias)
                (render-template post-template-name plist :data-db data-db)))
        aliases))))

(defun get-node-plists-for-index-page (&optional (db *data-db*))
  (with-settings (db)
    (let* ((post-count (get-setting :home-page-post-count))
           (res nil))
      (do-node-nums-before-time (node-num nil db)
        (let ((node (read-node node-num db)))
          (when (eql 1 (getf node :promote))
            (let* ((plist (make-node-plist node :comments-p nil :data-db db)))
              (setf (getf plist :permalink) (car (getf plist :aliases)))
              (let ((cnt 0))
                (dolist (comment-num (getf plist :comments))
                  (let ((comment-plist (read-comment comment-num db)))
                    (unless (eql 1 (getf comment-plist :status))
                      (incf cnt))))
                (when (> cnt 0)
                  (setf (getf plist :comment-count)
                        (if (eq cnt 1)
                            "1 comment"
                            (format nil "~d comments" cnt)))))
                 (push plist res)
                 (when (<= (decf post-count) 0)
                   (return))))))
      (nreverse res))))

(defun render-site-index (&key (data-db *data-db*) (site-db *site-db*))
  (let* ((node-plists (get-node-plists-for-index-page data-db))
         (my-links (multiple-value-bind (y m)
                       (decode-ymd (getf (car node-plists) :created))
                     (compute-months-and-years-link-plist y m data-db)))
         (plist `(:posts ,node-plists ,@my-links))
         (post-template-name (get-post-template-name data-db))
         (file-name "index.html"))
    (setf (getf plist :home) ".")
    (setf (fsdb:db-get site-db file-name)
          (render-template post-template-name plist :data-db data-db))
    file-name))
           
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
