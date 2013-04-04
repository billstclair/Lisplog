; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simple RSS reader
;;;

(in-package :lisplog)

(defclass rss ()
  ((title :accessor title :initarg :title :initform nil)
   (feed-link :accessor feed-link :initarg :feed-link :initform nil)
   (link :accessor link :initarg :link :initform nil)
   (base :accessor base :initarg :base :initform nil)
   (updated-time :accessor updated-time :initarg :updated-time :initform nil)
   (subtitle :accessor subtitle :initarg :subtitle :initform nil)
   (editor :accessor editor :initarg :editor :initform nil)
   (entries :accessor entries :initarg :entries :initform nil)
   (raw-parse :accessor raw-parse :initarg :raw-parse :initform nil)))

(defmethod print-object ((rss rss) stream)
  (print-unreadable-object (rss stream :type t)
    (format stream "~s ~s" (title rss) (feed-link rss))))

(defclass rss-entry ()
  ((rss :accessor rss :initarg :rss :initform nil)
   (title :accessor title :initarg :title :initform nil)
   (link :accessor link :initarg :link :initform nil)
   (id :accessor id :initarg :id :initform nil)
   (published-time :accessor published-time :initarg :published-time :initform nil)
   (pulished-time-string :accessor published-time-string
                         :initarg :published-time-string
                         :initform nil)
   (updated-time :accessor updated-time :initarg :updated-time :initform nil)
   (summary :accessor summary :initarg :summary :initform nil)
   (author :accessor author :initarg :author :initform nil)
   (author-uri :accessor author-uri :initarg :author-uri :initform nil)
   (content  :accessor content  :initarg :content  :initform nil)))

(defmethod print-object ((entry rss-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "~s" (title entry))))

(defun remove-namespaces-from-xml (xml)
  (when (and (listp xml)
             (consp (car xml))
             (stringp (caar xml))
             (stringp (cdar xml)))
    (setf (car xml) (caar xml)))
  (when (and (listp xml) (listp (cddr xml)))
    (mapc #'remove-namespaces-from-xml (cddr xml)))
  xml)

(defun cadr-assoc (key list &key (test #'equal))
  (cadr (assoc key list :test test)))

;; This currently groks two formats:
;; Atom: (parse-rss "http://www.antipope.org/charlie/blog-static/atom.xml")
;; Simple RSS: (parse-rss "http://billstclair.com/journal/rss.xml")
(defun parse-rss (url)
  (let* ((xml
          (let ((s (drakma:http-request
                    url
                    :want-stream t
                    :connection-timeout 5)))
            (unwind-protect
                 (remove-namespaces-from-xml
                  (or (xmls:parse s) (error "Empty RSS at ~s" url)))
              (close s))))
         (rss (make-instance 'rss
                             :feed-link url
                             :raw-parse xml)))
    ;; We ignore the outermost tag for now.
    ;; Its name and attributes might eventually be useful for something
    (when (and (listp xml)
               (listp (cdr xml)))
      (let ((base (cadr-assoc "base" (cadr xml))))
        (when base (setf (base rss) base)))
      (parse-rss-xml (cddr xml) rss))))

(defun parse-rss-xml (xml rss)
  (loop for element in xml
     for (tag attributes . body) = (and (listp element)
                                        (listp (cdr element))
                                        element)
     for value = (car body)
     do
       (cond ((null tag))
              ((equal tag "channel")  ;RSS, not ATOM
               (return-from parse-rss-xml (parse-rss-xml body rss)))
              ((equal tag "title")
               (when (stringp value)
                 (setf (title rss) value)))
              ((equal tag "link")
               (cond (attributes
                      (let ((href (cadr-assoc "href" attributes))
                            (type (cadr-assoc "type" attributes))
                            (rel (cadr-assoc "rel" attributes)))
                        (declare (ignore rel))
                        (when (equal type "text/html")
                          (setf (link rss) href))))
                     ((stringp value) (setf (link rss) value))))
              ((equal tag "updated")
               (when (stringp value)
                 (setf (updated-time rss) (parse-iso8601-time value))))
              ((or (equal tag "subtitle") (equal tag "description"))
               (when (stringp value)
                 (setf (subtitle rss) value)))
              ((equal tag "managingEditor")
               (when (stringp value)
                 (setf (editor rss) value)))
              ((or (equal tag "entry") (equal tag "item"))
               (push (parse-rss-entry rss body) (entries rss)))))
  (setf (entries rss) (nreverse (entries rss)))
  rss)

(defun parse-rss-entry (rss entry)
  (let ((res (make-instance 'rss-entry :rss rss)))
    (loop for element in entry
       for (tag attributes . body) = (and (listp element)
                                          (listp (cdr element))
                                          element)
       for value = (car body)
       do (cond ((null tag) nil)
                ((equal tag "title")
                 (when (stringp value)
                   (setf (title res) value)))
                ((equal tag "link")
                 (cond (attributes
                        (let ((href (cadr-assoc "href" attributes)))
                          (when (stringp href)
                            (setf (link res) href))))
                       ((stringp value) (setf (link res) value))))
                ((equal tag "id")
                 (when (stringp value)
                   (setf (id res) value)))
                ((equal tag "published")
                 (when (stringp value)
                   (setf (published-time-string res) value
                         (published-time res) (parse-iso8601-time value))))
                ((equal tag "updated")
                 (when (stringp value)
                   (setf (updated-time res) (parse-iso8601-time value))))
                ((equal tag "pubDate")
                 (when (stringp value)
                   (setf (published-time-string res) value
                         (published-time res) (parse-rfc-1123-date value))))
                ((equal tag "summary")
                 (when (stringp value)
                   (setf (summary res) value)))
                ((equal tag "creator")
                 (when (stringp value)
                   (setf (author res) value)))
                ((equal tag "author")
                 (loop for elt in body
                    for (tag attributes value) = (and (listp elt)
                                                      (listp (cdr elt))
                                                      elt)
                    do
                      (cond ((null tag) attributes)
                            ((equal tag "name")
                             (when (stringp value)
                               (setf (author res) value)))
                            ((equal tag "uri")
                             (when (stringp value)
                               (setf (author-uri res) value))))))
                ((or (equal tag "content")     ;ATOM
                     (equal tag "description") ;RSS
                     (equal tag "encoded"))    ;Wired
                 (when (and (stringp value)
                            (not (content res)))
                   (setf (content res) value)))))
    res))

;;;
;;; Database
;;;
;;; rss/
;;;   settings     ;; (:update-period <seconds>
;;;                ;;  :last-update <time>
;;;                ;;  :items-per-page <integer>
;;;                ;;  :current-page <integer>
;;;                ;;  :oldest-page <integer>
;;;                ;;  :max-pages <integer>)
;;;   feedurls     ;; (<url> <url> ...)
;;;   feeds/
;;;     <url-hash> ;; ((<url> :last-published-time <time>) ...)
;;;   index        ;; plist to generate index.html (latest page)
;;;

(defun rss-settings (&optional (db *data-db*))
  (sexp-get db $RSS $SETTINGS :subdirs-p nil))

(defun (setf rss-settings) (settings &optional (db *data-db*))
  (setf (sexp-get db $RSS $SETTINGS :subdirs-p nil) settings))

(defun rss-setting (key &optional (db *data-db*))
  (getf (rss-settings db) key))

(defun (setf rss-setting) (value key &optional (db *data-db*))
  (setf (getf (rss-settings db) key) value))

(defun rss-feedurls (&optional (db *data-db*))
  (sexp-get db $RSS $FEEDURLS :subdirs-p nil))
  
(defun (setf rss-feedurls) (urls &optional (db *data-db*))
  (setf (sexp-get db $RSS $FEEDURLS :subdirs-p nil) urls))

(defparameter $RSS/FEEDS (fsdb:append-db-keys $RSS $FEEDS))

(defun feed-hash-settings (hash &optional (db *data-db*))
  (sexp-get db $RSS/FEEDS hash :subdirs-p nil))

(defun (setf feed-hash-settings) (settings hash &optional (db *data-db*))
  (setf (sexp-get db $RSS/FEEDS hash :subdirs-p nil) settings))

(defun feed-settings (url &optional (db *data-db*))
  (cdr (assoc url (feed-hash-settings (cl-crypto:sha1 url) db)
              :test #'equal)))

(defun (setf feed-settings) (settings url &optional (db *data-db*))
  (let* ((hash (cl-crypto:sha1 url))
         (hash-settings (feed-hash-settings hash db))
         (cell (assoc url hash-settings :test #'equal)))
    (cond (cell (setf (cdr cell) settings))
          (t (push (cons url settings) hash-settings)))
    (setf (feed-hash-settings hash db) hash-settings)
    settings))

(defun feed-setting (url key &optional (db *data-db*))
  (getf (feed-settings url db) key))
  
(defun (setf feed-setting) (value url key &optional (db *data-db*))
  (setf (getf (feed-settings url db) key) value))
  
(defun rss-index (&optional (db *data-db*))
  (sexp-get db $RSS $INDEX :subdirs-p nil))

(defun (setf rss-index) (plist &optional (db *data-db*))
  (setf (sexp-get db $RSS $INDEX :subdirs-p nil) plist))

(defun clear-rss-settings (&optional (db *data-db*))
  (setf (rss-settings db) nil)
  (setf (rss-index db) nil)
  (dolist (url (rss-feedurls db))
    (setf (feed-settings url db) nil)))

;;;
;;; The aggregator
;;;

(defparameter *default-rss-update-period* 3600) ;1 hour
(defparameter *default-rss-items-per-page* 20)
(defparameter *default-rss-max-pages* 50)

(defun get-new-rss-entries (&key (db *data-db*)
                            (urls (rss-feedurls db)))
  (let* ((settings (rss-settings db))
         (current-page (or (getf settings :current-page) 1))
         (max-pages (or (getf settings :max-pages) *default-rss-max-pages*))
         (max-published-times nil)
         (entries nil))
    current-page max-pages
    (dolist (url urls)
      (format t "~s~%" url)
      (let* ((last-published-time (or (feed-setting url :last-published-time db) 0))
             (max-published-time last-published-time)
             (new-entries nil)
            (rss (ignore-errors (parse-rss url))))
        (when rss
          (dolist (entry (entries rss))
            (let ((published-time (published-time entry)))
              (when (and published-time (> published-time last-published-time))
                (push entry new-entries)
                (when (> published-time max-published-time)
                  (setf max-published-time published-time)))))
          (when new-entries
            (setf entries (merge 'list
                                 entries
                                 (sort new-entries #'> :key #'published-time)
                                 #'>
                                 :key #'published-time))
            (push (cons url max-published-time) max-published-times)))))
    (values entries max-published-times)))

(defparameter *style-rss-post-file* ".rss-post.tmpl")

(defun get-rss-post-template-name (&optional (db *data-db*))
  (with-settings (db)
    (or (get-setting :rss-post-template) *style-rss-post-file*)))

;; If their xml contains " srcnot='http:", we'll lose.
;; Let them report it as a bug.
(defun insert-rss-base (base body)
  (let ((replacement (format nil "\\1~a" base)))
    (setf body (cl-ppcre:regex-replace-all
                " srcnot=(['\"]https?:)"
                (cl-ppcre:regex-replace-all
                 "( src=['\"])"
                 (cl-ppcre:regex-replace-all
                  " src=(['\"]https?:)" body " srcnot=\\1")
                 replacement)
                " src=\\1"))
    (cl-ppcre:regex-replace-all
     " hrefnot=(['\"]https?:)"
     (cl-ppcre:regex-replace-all
      "( href=['\"])"
      (cl-ppcre:regex-replace-all
       " href=(['\"]https?:)" body " hrefnot=\\1")
      replacement)
     " href=\\1")))

(defun make-rss-entry-plist (entry)
  (let* ((rss (rss entry))
         (body (or (summary entry) (content entry)))
         (base (base rss)))
    (when base
      (setf body (insert-rss-base base body)))
    `(:permalink ,(link entry)
      :title ,(title entry)
      :site-link ,(link rss)
      :feed-link ,(feed-link rss)
      :site-name ,(title rss)
      :author ,(author entry)
      :post-date ,(hunchentoot:rfc-1123-date (published-time entry))
      :body ,body)))

(defun rss-page-number-to-alias (page-number &optional no-dir-p)
  (format nil "~a~d.html"
          (if no-dir-p "" "aggregator/")
          page-number))

(defun render-rss-page (page-number posts &key
                        first-p (data-db *data-db*) (site-db *site-db*))
  (let ((oddp t))
    (loop for tail on posts
       for post = (car tail)
       do
         (setf (getf post :oddp) oddp
               (car tail) post
               oddp (not oddp))))
  (let* ((alias (rss-page-number-to-alias page-number))
         (permalink (rss-page-number-to-alias page-number t))
         (older-link (and (> page-number 1)
                          (rss-page-number-to-alias (1- page-number) t)))
         (newer-link (unless first-p
                       (rss-page-number-to-alias (1+ page-number) t)))
         (post-template-name (get-rss-post-template-name data-db))
         (plist `(:posts ,posts
                  :post-date ,(hunchentoot:rfc-1123-date)
                  :title ,(if first-p
                              "Feed Aggregator"
                              (format nil "Feed Aggregator Page ~d"
                                      page-number))
                  :home ,(determine-home alias)
                  :permalink ,(if first-p "./" permalink)
                  :prev-url ,newer-link
                  :next-url ,older-link)))
    (setf (fsdb:db-get site-db alias)
          (render-template post-template-name plist :data-db data-db))
    alias))

(defun trim-rss-pages (&optional (data-db *data-db*) (site-db *site-db*))
  (let* ((settings (rss-settings data-db))
         (max-pages (or (getf settings :max-pages) *default-rss-max-pages*))
         (oldest-page (getf settings :oldest-page)))
    ;; To do
    max-pages oldest-page site-db))

(defun render-rss-pages (entries &key (data-db *data-db*) (site-db *site-db*))
  (unless entries
    (return-from render-rss-pages))

  (let* ((old-posts (rss-index data-db))
         (settings (rss-settings data-db))
         (items-per-page (or (getf settings :items-per-page)
                             *default-rss-items-per-page*))
         (current-page (or (getf settings :current-page) 1))
         (all-posts (nconc (mapcar #'make-rss-entry-plist entries) old-posts))
         (post-cnt (length all-posts))
         (pages (ceiling post-cnt items-per-page))
         (first-page-cnt (- post-cnt (* (1- pages) items-per-page)))
         (first-page-posts (subseq all-posts 0 first-page-cnt))
         (last-page (+ current-page pages -1)))
    (loop for page-number from last-page downto current-page
       for page-posts = first-page-posts then (subseq posts 0 items-per-page)
       for posts = (nthcdr first-page-cnt all-posts)
       then (nthcdr items-per-page posts)
       do
         (render-rss-page page-number page-posts
                          :first-p (eql page-number last-page)
                          :data-db data-db))
    (cl-fad:copy-file
     (fsdb:db-filename site-db (rss-page-number-to-alias last-page))
     (fsdb:db-filename site-db (rss-page-number-to-alias "index"))
     :overwrite t)
    (setf (rss-setting :current-page data-db) last-page
          (rss-index data-db) first-page-posts)
    (trim-rss-pages data-db site-db)))

(defun aggregate-rss (&key (data-db *data-db*)
                      (site-db *site-db*)
                      (urls (rss-feedurls data-db)))
  (multiple-value-bind (entries max-published-time-alist)
      (get-new-rss-entries :urls urls :db data-db)
    (render-rss-pages entries :data-db data-db :site-db site-db)
    (loop for (url . time) in max-published-time-alist
       do
         (setf (feed-setting url :last-published-time data-db) time))
    (setf (rss-setting :last-update) (get-universal-time))
    (length entries)))

(defvar *rss-reader-thread* nil)

(defun start-rss-reader-thread ()
  (unless *rss-reader-thread*
    (bt:make-thread #'rss-reader-thread-loop :name "RSS Reader")))

(defun rss-reader-thread-loop ()
  (unless *rss-reader-thread* 
    (setf *rss-reader-thread* (bt:current-thread))
    (unwind-protect
         (loop
            (ignore-errors (rss-reader-thread-step))
            (sleep 60))
      (setf *rss-reader-thread* nil))))

(defun rss-reader-thread-step ()
  (do-port-dbs (db)
    (let ((urls (rss-feedurls db)))
      (when urls
        (let* ((settings (rss-settings))
               (last-update (or (getf settings :last-update) 0))
               (update-period (or (getf settings :update-period)
                                  *default-rss-update-period*))
               (next-update (+ last-update update-period)))
          (when (>= (get-universal-time) next-update)
            (format t "Aggregating RSS for ~s~%"
                    (with-settings () (get-setting :site-name)))
            (aggregate-rss :urls urls)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2013 Bill St. Clair
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
