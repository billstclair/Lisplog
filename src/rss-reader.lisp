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
   (image :accessor image :initarg :image :initform nil)
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
                    :connection-timeout 5
                    #+ccl :deadline
                    #+ccl (+ (get-internal-real-time)
                             (* 30 internal-time-units-per-second)))))
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
     for stringp = (stringp value)
     do
       (cond ((null tag))
              ((equal tag "channel")  ;RSS, not ATOM
               (return-from parse-rss-xml (parse-rss-xml body rss)))
              ((equal tag "title")
               (when stringp
                 (setf (title rss) value)))
              ((equal tag "link")
               (cond (attributes
                      (let ((href (cadr-assoc "href" attributes))
                            (type (cadr-assoc "type" attributes))
                            (rel (cadr-assoc "rel" attributes)))
                        (declare (ignore rel))
                        (when (equal type "text/html")
                          (setf (link rss) href))))
                     (stringp (setf (link rss) value))))
              ((equal tag "updated")
               (when stringp
                 (setf (updated-time rss) (parse-iso8601-time value))))
              ((or (equal tag "subtitle") (equal tag "description"))
               (when stringp
                 (setf (subtitle rss) value)))
              ((equal tag "managingEditor")
               (when stringp
                 (setf (editor rss) value)))
              ((or (equal tag "image")
                   (and (equal tag "avatar") (not (image rss))))
               (cond (stringp
                      (setf (image rss) value))
                     (t (let ((url (assoc "url" body :test #'equal)))
                          (when (and (consp (cdr url))
                                     (listp (cddr url))
                                     (stringp (setf url (caddr url))))
                            (setf (image rss) url))))))
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
       for stringp = (stringp value)
       do (cond ((null tag) nil)
                ((equal tag "title")
                 (when stringp
                   (setf (title res) value)))
                ((equal tag "link")
                 (cond (attributes
                        (let ((href (cadr-assoc "href" attributes)))
                          (when (stringp href)
                            (setf (link res) href))))
                       (stringp (setf (link res) value))))
                ((equal tag "id")
                 (when stringp
                   (setf (id res) value)))
                ((equal tag "published")
                 (when stringp
                   (setf (published-time-string res) value
                         (published-time res) (parse-iso8601-time value))))
                ((equal tag "updated")
                 (when stringp
                   (setf (updated-time res) (parse-iso8601-time value))))
                ((equal tag "pubDate")
                 (when stringp
                   (setf (published-time-string res) value
                         (published-time res) (parse-rfc-1123-date value))))
                ((equal tag "summary")
                 (when stringp
                   (setf (summary res) value)))
                ((equal tag "creator")
                 (when stringp
                   (setf (author res) value)))
                ((equal tag "author")
                 (loop for elt in body
                    for (tag attributes value) = (and (listp elt)
                                                      (listp (cdr elt))
                                                      elt)
                    for stringp = (stringp value)
                    do
                      (cond ((null tag) attributes)
                            ((equal tag "name")
                             (when stringp
                               (setf (author res) value)))
                            ((equal tag "uri")
                             (when stringp
                               (setf (author-uri res) value))))))
                ((or (equal tag "content")     ;ATOM
                     (equal tag "description") ;RSS
                     (equal tag "encoded"))    ;Wired
                 (when (and stringp (not (content res)))
                   (setf (content res) value)))))
    (unless (published-time res)
      (determine-published-time rss res))
    (when (and (published-time res)
               (not (updated-time rss)))
      (setf (updated-time rss) (published-time res)))
    res))

;; Tricks to get times for feeds without the proper fields
;; This currently works for only the GoComics.com date in title feed
(defun determine-published-time (rss entry)
  (declare (ignore rss))
  (let ((title (title entry)))
    (when title
      (let ((day-date-year (split-sequence:split-sequence #\, title)))
        (when (eql 3 (length day-date-year))
          (let ((month-day (split-sequence:split-sequence
                            #\space (fsdb:trim (second day-date-year))))
                (year (fsdb:trim (third day-date-year))))
            (when (eql 2 (length month-day))
              (let ((month (first month-day))
                    (day (second month-day)))
                (ignore-errors
                  (setf month (1+ (position month *month-names*
                                            :test #'string-equal))
                        day (parse-integer day)
                        year (parse-integer year))
                  (setf (published-time-string entry)
                        title
                        (published-time entry)
                        (encode-universal-time 0 0 0 day month year)))))))))))


;;;
;;; Database
;;;
;;; rss/
;;;   settings     ;; (:updates-per-hour <integer>
;;;                ;;  :last-update <time>
;;;                ;;  :items-per-page <integer>
;;;                ;;  :current-page <integer>
;;;                ;;  :oldest-page <integer>
;;;                ;;  :max-pages <integer>)
;;;   feedurls     ;; (<url> <url> ...)
;;;   feeds/
;;;     <url-hash> ;; ((<url> :last-published-time <time>
;;;                           :title <string>
;;;                           :subtitle <string>
;;;                           :editor <string>
;;;                           :link <string>
;;;                           :image <string>
;;;                ;;  )
;;;                ;;  ...)
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

(defun feed-settings (url &key
                      (db *data-db*)
                      hash
                      (hash-settings nil hash-settings-p))
  (unless hash-settings-p
    (unless hash (setf hash (cl-crypto:sha1 url)))
    (setf hash-settings (feed-hash-settings hash db)))
  (values (cdr (assoc url hash-settings :test #'equal))
          hash-settings
          hash))

(defun (setf feed-settings) (settings url &key
                             (db *data-db*)
                             hash
                             (hash-settings nil hash-settings-p))
  (unless hash (setf hash (cl-crypto:sha1 url)))
  (unless hash-settings-p
    (setf hash-settings (feed-hash-settings hash db)))
  (let* ((cell (assoc url hash-settings :test #'equal)))
    (cond (cell (cond ((null settings)
                       (setf hash-settings (delete url hash-settings
                                                   :test #'equal
                                                   :key #'car)))
                      (t (setf (cdr cell) settings))))
          (settings (push (cons url settings) hash-settings)))
    (setf (feed-hash-settings hash db) hash-settings)
    settings))

(defun feed-setting (url key &optional (db *data-db*))
  (getf (feed-settings url :db db) key))
  
(defun (setf feed-setting) (value url key &optional (db *data-db*))
  (setf (getf (feed-settings url :db db) key) value))
  
(defun rss-index (&optional (db *data-db*))
  (sexp-get db $RSS $INDEX :subdirs-p nil))

(defun (setf rss-index) (plist &optional (db *data-db*))
  (setf (sexp-get db $RSS $INDEX :subdirs-p nil) plist))

(defun clear-rss-settings (&optional (db *data-db*))
  (setf (rss-settings db) nil)
  (setf (rss-index db) nil)
  (dolist (url (rss-feedurls db))
    (setf (feed-settings url :db db) nil)))

;;;
;;; The aggregator
;;;

(defparameter *default-rss-updates-per-hour* 2)
(defparameter *default-rss-items-per-page* 20)
(defparameter *default-rss-max-pages* 50)

(defun save-rss-settings (rss &optional (db *data-db*))
  (let* ((url (feed-link rss)))
    (multiple-value-bind (settings hash-settings hash)
        (feed-settings url :db db)
      (flet ((setit (key value)
               ;; Maybe I should just set here.
               ;; This makes it remember deleted values.
               (when value
                 (setf (getf settings key) value))))
        (setit :title (title rss))
        (setit :subtitle (subtitle rss))
        (setit :editor (editor rss))
        (setit :link (link rss))
        (setit :image (image rss)))
      (setf (feed-settings url :db db :hash hash :hash-settings hash-settings)
            settings))))

(defun get-new-rss-entries (&key (db *data-db*)
                            (urls (rss-feedurls db)))
  (let* ((settings (rss-settings db))
         (current-page (or (getf settings :current-page) 1))
         (max-pages (or (getf settings :max-pages) *default-rss-max-pages*))
         (max-published-times nil)
         (entries nil))
    current-page max-pages
    (flet ((process-url (url)
             (let* ((last-published-time
                     (or (feed-setting url :last-published-time db) 0))
                    (max-published-time last-published-time)
                    (new-entries nil)
                    (new-entry-cnt 0))
               (multiple-value-bind (rss err)
                   (ignore-errors (parse-rss url))
                 (when rss
                   (save-rss-settings rss db)
                   (dolist (entry (entries rss))
                     (let ((published-time (published-time entry)))
                       (when (and published-time
                                  (> published-time last-published-time))
                         (push entry new-entries)
                         (when (> published-time max-published-time)
                           (setf max-published-time published-time)))))
                   (when new-entries
                     (setf new-entries (sort new-entries #'> :key #'published-time)
                           new-entry-cnt (length new-entries)
                           entries (merge 'list entries new-entries #'>
                                          :key #'published-time))
                     (push (cons url max-published-time) max-published-times)))
                 (values new-entry-cnt err)))))
          (dolist (url urls)
            (format t "~s" url)
            (let ((done nil))
              (unwind-protect
                   (loop
                      (restart-case
                          (multiple-value-bind (cnt err) (process-url url)
                            (unless err
                              (format t " ~d~%" cnt)
                              (setf done t))
                            (return))
                        (cancel-processing ()
                          :report (lambda (stream)
                                    (format stream "Cancel processing ~a" url))
                          (return))
                        (retry-processing ()
                          :report (lambda (stream)
                                    (format stream "Retry processing ~a" url)))))
                (unless done
                  (format t " ***ERROR***~%"))))))
    (values entries max-published-times)))

(defparameter *style-rss-post-file* ".rss-post.tmpl")

(defun get-rss-post-template-name (&optional (db *data-db*))
  (with-settings (db)
    (or (get-setting :rss-post-template) *style-rss-post-file*)))

(defun rss-post-template-p (&optional (db *data-db*))
  (let ((template-name (get-rss-post-template-name db)))
    (and template-name
         (get-style-file template-name db)
         template-name)))

;; If their xml contains " srcnot='http:", we'll lose.
;; Let them report it as a bug.
(defun insert-rss-base (base body)
  (let ((replacement (format nil "\\1~a" base)))
    (setf body (cl-ppcre:regex-replace-all
                " src=(['\"]https?:)" body " srcnot=\\1"))
    (setf body (cl-ppcre:regex-replace-all
                 "( src=['\"])" body replacement))
    (setf body (cl-ppcre:regex-replace-all
                " srcnot=(['\"]https?:)" body " src=\\1"))

    (setf body (cl-ppcre:regex-replace-all
                " href=(['\"]mailto:)" body " hrefnot=\\1"))
    (setf body (cl-ppcre:regex-replace-all
                " href=(['\"]https?:)" body " hrefnot=\\1"))
    (setf body (cl-ppcre:regex-replace-all
                "( href=['\"])" body replacement))
    (setf body (cl-ppcre:regex-replace-all
                " hrefnot=(['\"]https?:)" body " href=\\1"))
    (setf body (cl-ppcre:regex-replace-all
                " hrefnot=(['\"]mailto:)" body " href=\\1"))

    body))

(defun make-rss-entry-plist (entry)
  (let* ((rss (rss entry))
         (body (or (summary entry) (content entry)))
         (base (base rss))
         (title (title entry)))
    (when base
      (setf body (insert-rss-base base body)))
    `(:permalink ,(link entry)
      :title ,(if (blankp title) "No Title" title)
      :site-link ,(link rss)
      :feed-link ,(feed-link rss)
      :site-name ,(title rss)
      :author ,(author entry)
      :post-date ,(hunchentoot:rfc-1123-date (published-time entry))
      :image ,(image rss)
      :body ,body)))

(defun rss-page-number-to-alias (page-number &optional no-dir-p)
  (format nil "~a~d.html"
          (if no-dir-p "" "aggregator/")
          page-number))

(defvar *rss-default-page-title* "Feed Aggregator")

(defun rss-page-title (&optional (db *data-db*))
  (with-settings (db)
    (or (get-setting :rss-page-title) *rss-default-page-title*)))

(defun render-rss-page (page-number posts &key
                        first-p (data-db *data-db*) (site-db *site-db*)
                        last-p note next-update)
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
                          (not last-p)
                          (rss-page-number-to-alias (1- page-number) t)))
         (newer-link (unless first-p
                       (rss-page-number-to-alias (1+ page-number) t)))
         (post-template-name (get-rss-post-template-name data-db))
         (title (if first-p
                    (rss-page-title data-db)
                    (format nil "~a Page ~d"
                            (rss-page-title data-db) page-number)))
         (plist `(:posts ,posts
                  :post-date ,(hunchentoot:rfc-1123-date)
                  :title ,title
                  :page-title ,title
                  :note ,note
                  :next-update ,next-update
                  :home ,(determine-home alias)
                  :permalink ,(if first-p "./" permalink)
                  :prev-url ,newer-link
                  :next-url ,older-link)))
    (setf (fsdb:db-get site-db alias)
          (render-template post-template-name plist :data-db data-db))
    alias))

(defun rss-feeds-alias (&optional no-dir-p)
  (format nil "~a~a"
          (if no-dir-p "" "aggregator/")
          "feeds.html"))

(defparameter *style-rss-feeds-file* ".rss-feeds.tmpl")

(defun get-rss-feeds-template-name (&optional (db *data-db*))
  (with-settings (db)
    (or (get-setting :rss-feeds-template) *style-rss-feeds-file*)))

(defun rss-feeds-template-p (&optional (db *data-db*))
  (let ((template-name (get-rss-feeds-template-name db)))
    (and template-name
         (get-style-file template-name db)
         template-name)))

(defun render-rss-feeds (&key (data-db *data-db*) (site-db *site-db*))
  (unless (rss-feeds-template-p data-db)
    (return-from render-rss-feeds nil))
  (let* ((urls (rss-feedurls data-db))
         (lines (loop for url in urls
                   for settings = (feed-settings url :db data-db)
                   when settings
                   collect (let ((last-published
                                  (getf settings :last-published-time)))
                             (when last-published
                               (setf (getf settings :last-published-time)
                                     (hunchentoot:rfc-1123-date last-published)))
                             `(:url ,url ,@settings))))
         (feeds-template-name (get-rss-feeds-template-name data-db))
         (title "RSS Feeds")
         (alias (rss-feeds-alias))
         (permalink (rss-feeds-alias t))
         (plist `(:lines ,(sort lines #'string-lessp
                                :key (lambda (x) (or (getf x :title) "z")))
                  :post-date ,(hunchentoot:rfc-1123-date)
                  :title ,title
                  :page-title ,title
                  :permalink ,permalink
                  :home ,(determine-home alias))))
    (setf (fsdb:db-get site-db alias)
          (render-template feeds-template-name plist :data-db data-db))
    alias))

(defun trim-rss-pages (&optional (data-db *data-db*) (site-db *site-db*))
  (let* ((settings (rss-settings data-db))
         (max-pages (or (getf settings :max-pages) *default-rss-max-pages*))
         (current-page (getf settings :current-page))
         (oldest-page (or (getf settings :oldest-page) 1))
         (delcnt (- (- current-page (1- oldest-page)) max-pages)))
    (when (> delcnt 0)
      (let ((blank-page (+ oldest-page delcnt -1))
            (total-items (* max-pages
                            (or (getf settings :items-per-page)
                                *default-rss-items-per-page*))))
        (render-rss-page blank-page nil
                         :last-p t
                         :note (format nil "End of ~d saved items." total-items)
                         :data-db data-db
                         :site-db site-db))
      (decf oldest-page)                ;delete last off-end page
      (loop for i from 0 below delcnt
         for alias = (rss-page-number-to-alias oldest-page)
         do
           (setf (fsdb:db-get site-db alias) nil)
           (incf oldest-page))
      (incf oldest-page)                  ;skip blank page
      (setf (rss-setting :oldest-page data-db) oldest-page))))

(defun render-rss-pages (entries &key (data-db *data-db*) (site-db *site-db*))
  (let* ((old-posts (rss-index data-db))
         (settings (rss-settings data-db))
         (items-per-page (or (getf settings :items-per-page)
                             *default-rss-items-per-page*))
         (current-page (or (getf settings :current-page) 1))
         (all-posts (nconc (mapcar #'make-rss-entry-plist entries) old-posts))
         (post-cnt (length all-posts))
         (pages (ceiling post-cnt items-per-page))
         (first-page-cnt (- post-cnt (* (max 0 (1- pages)) items-per-page)))
         (first-page-posts (subseq all-posts 0 first-page-cnt))
         (last-page (+ current-page pages -1))
         (next-update (rss-next-update :db data-db)))
    (loop for page-number from last-page downto current-page
       for page-posts = first-page-posts then (subseq posts 0 items-per-page)
       for posts = (nthcdr first-page-cnt all-posts)
       then (nthcdr items-per-page posts)
       do
         (render-rss-page page-number page-posts
                          :first-p (eql page-number last-page)
                          :next-update (and next-update
                                            (hunchentoot:rfc-1123-date next-update))
                          :data-db data-db
                          :site-db site-db)
         (setf next-update nil))
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
  ;; Prevent update thread from interfering
  (setf (rss-setting :last-update data-db) (get-universal-time))
  (unless (rss-post-template-p data-db)
    (return-from aggregate-rss 0))
  (multiple-value-bind (entries max-published-time-alist)
      (get-new-rss-entries :urls urls :db data-db)
    (render-rss-pages entries :data-db data-db :site-db site-db)
    (loop for (url . time) in max-published-time-alist
       do
         (setf (feed-setting url :last-published-time data-db) time))
    (setf (rss-setting :last-update data-db) (get-universal-time))
    (when (rss-feeds-template-p data-db)
      (render-rss-feeds :data-db data-db :site-db site-db))
    (length entries)))

(defvar *rss-reader-thread* nil)

(defun kill-rss-reader-thread ()
  (let ((thread *rss-reader-thread*))
    (when thread
      (setf *rss-reader-thread* nil)
      (bt:destroy-thread thread)
      thread)))

(defun start-rss-reader-thread (&optional restart-p)
  (when restart-p
    (kill-rss-reader-thread))
  (let ((thread *rss-reader-thread*))
    (unless (and thread (bt:thread-alive-p thread))
      (bt:make-thread #'rss-reader-thread-loop :name "RSS Reader"))))

(defun rss-reader-thread-loop ()
  (unless *rss-reader-thread* 
    (setf *rss-reader-thread* (bt:current-thread))
    (unwind-protect
         (loop (rss-reader-thread-loop-body))
      (setf *rss-reader-thread* nil))))

(defun rss-reader-thread-loop-body ()
  (let ((sleep-time 60))
    (let ((next-update (ignore-errors (rss-reader-thread-step))))
      (when next-update
        (let ((delay (- next-update (get-universal-time))))
          (when (< delay sleep-time)
            (setf sleep-time (max 0 delay))))))
    (restart-case
        (sleep sleep-time)
      (stop-sleeping ()
        :report (lambda (stream)
                  (format stream "stop-sleeping"))))))

(defun rss-next-update (&key last-update updates-per-hour (db *data-db*))
  (let ((settings (rss-settings db)))
    (unless last-update
      (setf last-update (or (getf settings :last-update) 0)))
    (unless updates-per-hour
      (setf updates-per-hour (or (getf settings :updates-per-hour)
                                 *default-rss-updates-per-hour*))))
  (multiple-value-bind (lsec lmin)
      (decode-universal-time last-update 0)
    (loop with minutes-per-update = (/ 60 updates-per-hour)
       for minute from 0 by minutes-per-update
       do
         (when (> minute lmin)
           (return (+ last-update (- (* minute 60) (+ (* lmin 60) lsec))))))))

(defun rss-reader-thread-step ()
  (let ((did-one? nil)
        (min-next-update nil))
    (do-port-dbs (db)
      (let ((urls (rss-feedurls db)))
        (when urls
          (let* ((next-update (rss-next-update :db db)))
            (when (>= (get-universal-time) next-update)
              (setf did-one? t)
              (format t "~%Aggregating RSS for ~s~%"
                      (with-settings () (get-setting :site-name)))
              (format t "~a~%" (hunchentoot:rfc-1123-date))
              (let ((count (aggregate-rss :urls urls)))
                (format t "~d new entries~%" count))
              (setf next-update (rss-next-update :db db)))
            (when (or (null min-next-update) (< next-update min-next-update))
              (setf min-next-update next-update))))))
    (when did-one?
      (format t "Done aggregating RSS feeds~%"))
    min-next-update))

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
