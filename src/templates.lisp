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
  (setf (node-get db dir file :subdirs-p subsirs-p) value))

(defun fill-and-print-to-string (template values)
  (with-output-to-string (stream)
    (let ((template:*string-modifier* 'identity))
      (template:fill-and-print-template template values :stream stream))))

(defun unix-time-to-rfc-1123-string (&optional unix-time)
  (hunchentoot:rfc-1123-date
   (if unix-time
       (unix-to-universal-time unix-time)
       (get-universal-time))))

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

(defun drupal-format (str)
  (eliminate-empty-paragraphs
   (do-drupal-line-breaks
       (do-drupal-quotes str))))

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
     do
       (setf (getf plist :comment) (drupal-format text)
             (getf plist :post-date)
             (unix-time-to-rfc-1123-string (getf plist :timestamp)))
       (when (blankp (getf plist :homepage))
         (setf (getf plist :homepage) nil))
     collect plist))

(defun render-template (template-name plist &key
                        (data-db *data-db*) index-template-name)
  (with-settings ()
    (let* ((template (get-style-file template-name data-db))
           (index-template
            (get-style-file (or index-template-name *style-index-file*) data-db)))
      (setf plist (append plist *settings*))
      (unless (getf plist :home) (setf (getf plist :home) "."))
      (setf (getf plist :blocks) (get-blocks))
      (setf (getf plist :page-content)
            (fill-and-print-to-string template plist))
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

(defun make-node-plist (node &key (data-db *data-db*))
  (with-settings ()
    (let* ((plist (or (if (listp node) node (data-get $NODES node :db data-db))
                      (error "Node does not exist: ~s" node)))
           (created (getf plist :created))
           (status (getf plist :status))
           (uid (getf plist :uid))
           (format (getf plist :format))
           (user-plist (data-get $USERS uid :db data-db)))
      (when (eql status 1)
        (setf (getf plist :page-title) (getf plist :title))
        (unless (eql format 6)
          (setf plist (do-drupal-formatting plist)))
        (setf (getf plist :post-date)
              (unix-time-to-rfc-1123-string created))
        (setf (getf plist :author) (getf user-plist :name))
        (setf (getf plist :comment-plists)
              (fetch-comments (getf plist :comments) data-db))
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
                      ,@(compute-history-plist node data-db)))
        (dolist (alias aliases)
          ;; This needs to change based on the path in each alias
          (setf (getf plist :home) (determine-home alias))
          (setf (getf plist :permalink) alias)
          (setf (fsdb:db-get site-db alias)
                (render-template post-template-name plist :data-db data-db)))
        aliases))))

(defun render-site-index (&key (data-db *data-db*) (site-db *site-db*))
  (with-settings ()
    (let* ((post-count (get-setting :home-page-post-count))
           (post-template-name (get-post-template-name data-db))
           (node-nums (get-node-nums-before-time post-count nil data-db))
           (node-plists (mapcar (lambda (node)
                                  (let ((plist (make-node-plist
                                                node :data-db data-db)))
                                    (setf (getf plist :permalink)
                                          (car (getf plist :aliases)))
                                    plist))
                                node-nums))
           (plist `(:posts
                    ,node-plists
                    ,@(multiple-value-bind (y m)
                         (decode-ym (getf (car node-plists) :created))
                       (compute-months-and-years-link-plist y m data-db))))
           (file-name "index.html"))
      (setf (getf plist :home) ".")
      (setf (fsdb:db-get site-db file-name)
            (render-template post-template-name plist :data-db data-db))
      file-name)))
           
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
