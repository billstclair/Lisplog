; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Month and year pages, and last n posts
;;;

(in-package :lisplog)

(defun downcase-string-hash (string)
  (md5 (string-downcase string)))

(defun read-usernamehash (username &optional (db *data-db*))
  (let ((hash (downcase-string-hash username)))
    (data-get $USERNAMEHASH hash :db db)))

(defun (setf read-usernamehash) (value username &optional (db *data-db*))
  (let ((hash (downcase-string-hash username)))
    (setf (data-get $USERNAMEHASH hash :db db) value)))

(defun add-user-to-usernamehash (user &optional (db *data-db*))
  (let* ((name (getf user :name))
         (uid (getf user :uid)))
    (when (and name uid)
      (let ((uids (read-usernamehash name db)))
        (pushnew uid uids)
        (setf (read-usernamehash name db) uids)))))

(defun get-user-by-name (username &optional (db *data-db*))
  (maybe-index-users db)
  (let ((uids (read-usernamehash username db)))
    (dolist (uid uids)
      (let ((user (read-user uid db)))
        (when (and user (equalp username (getf user :name)))
          (return user))))))

(defun remove-username-from-usernamehash (username uid &optional (db *data-db*))
  (let ((uids (delete uid (read-usernamehash username db))))
    (setf (read-usernamehash username db) uids)))

(defun read-emailhash (email &optional (db *data-db*))
  (let ((hash (downcase-string-hash email)))
    (data-get $EMAILHASH hash :db db)))

(defun (setf read-emailhash) (value email &optional (db *data-db*))
  (let ((hash (downcase-string-hash email)))
    (setf (data-get $EMAILHASH hash :db db) value)))

(defun add-user-to-emailhash (user &optional (db *data-db*))
  (let* ((email (getf user :mail))
         (uid (getf user :uid)))
    (when (and email uid)
      (let ((uids (read-emailhash email db)))
        (pushnew uid uids)
        (setf (read-emailhash email db) uids)))))

(defun get-user-by-email (email &optional (db *data-db*))
  (maybe-index-users db)
  (setf email (string-downcase email))
  (let ((uids (read-emailhash email db)))
    (dolist (uid uids)
      (let ((user (read-user uid db)))
        (when (and user (equalp email (getf user :mail)))
          (return user))))))

(defun remove-email-from-emailhash (email uid &optional (db *data-db*))
  (let ((uids (delete uid (read-emailhash email db))))
    (setf (read-emailhash email db) uids)))

(defun index-users (&optional (db *data-db*))
  (do-users (user db)
    (add-user-to-usernamehash user db)
    (add-user-to-emailhash user db)))

(defun maybe-index-users (&optional (db *data-db*))
  (unless (and (aand (probe-file (fsdb:db-filename db $USERNAMEHASH))
                     (cl-fad:directory-pathname-p it))
               (aand (probe-file (fsdb:db-filename db $EMAILHASH))
                     (cl-fad:directory-pathname-p it)))
    (index-users)))

;; Should do something to posts with status other than 1 here.
;; Then we'll be able to view them in the admin interface.
(defun decode-ymd (unix-time)
  (multiple-value-bind (s min h d m y)
      (decode-universal-time
       (if unix-time
           (unix-to-universal-time unix-time)
           (get-universal-time)))
    (declare (ignore s min h))
    (values y m d)))

(defun index-years (&optional (db *data-db*))
  (cl-fad:delete-directory-and-files
   (fsdb:db-filename db $YEARS) :if-does-not-exist :ignore)
  (let ((last-y 0))
    (do-nodes (node db)
      (when (eql 1 (getf node :status))
        (let* ((nid (getf node :nid))
               (created (getf node :created)))
          (multiple-value-bind (y m) (decode-ymd created)
            (when (> y last-y)
              (format t "~d " y)
              (setf last-y y))
            (let ((ys (prin1-to-string y))
                  (ms (prin1-to-string m)))
              (let ((posts (fsdb:db-get db $YEARS ys ms)))
                (when posts
                  (setf posts (read-from-string posts)))
                (setf (fsdb:db-get db $YEARS ys ms)
                      (prin1-to-string
                       (merge 'list posts `((,nid . ,created))
                              #'<
                              :key #'cdr)))))))))
    (terpri)))

(defun update-node-year-index (node &optional (db *data-db*))
  (when (integerp node)
    (setf node (read-node node db)))
  (let ((created (getf node :created))
        (status (getf node :status)))
    (multiple-value-bind (y m) (decode-ymd created)
      (let* ((ys (prin1-to-string y))
             (ms (prin1-to-string m))
             (nid (getf node :nid))
             (posts (awhen (fsdb:db-get db $YEARS ys ms)
                      (read-from-string it)))
             (cell (assoc nid posts)))
        (cond ((eql status 1)
               (unless (and cell (eql (cdr cell) created))
                 (setf posts (delete nid posts :key #'car))
                 (when (eql 1 (getf node :status))
                   (setf posts (merge 'list posts `((,nid . ,created))
                                      #'< :key #'cdr)))
                 (setf (fsdb:db-get db $YEARS ys ms) (prin1-to-string posts)))
               (values y m))
              (cell
               (setf posts (delete nid posts :key #'car)
                     (fsdb:db-get db $YEARS ys ms) (prin1-to-string posts))
               (values y m)))))))

(defun get-years-before-time (unix-time &optional (db *data-db*))
  "Return a list of this year and older years that have posts,
as integers."
  (get-years-before-year (decode-ymd unix-time) db))

(defun get-years-before-year (y &optional (db *data-db*))
  "Return a list of years up to Y that have posts"
  (sort
   (loop for year-str in (fsdb:db-contents db $YEARS)
      for year = (ignore-errors (parse-integer year-str))
      when (and year (or (null y) (<= year y)))
      collect year)
   #'>))
         
(defun get-years-after-year (y &optional (db *data-db*))
  "Return a list of years up to Y that have posts"
  (sort
   (loop for year-str in (fsdb:db-contents db $YEARS)
      for year = (ignore-errors (parse-integer year-str))
      when (and year (>= year y))
      collect year)
   #'<))
         
(defun get-months-of-year (y &optional (db *data-db*))
  (sort
   (loop for month-str in (fsdb:db-contents db $YEARS (prin1-to-string y))
      for month = (ignore-errors (parse-integer month-str))
      when month
      collect month)
   #'>))

(defun get-month-post-info (year month &key (db *data-db*) (sort-predicate #'>))
  (let ((str (fsdb:db-get
              db $YEARS (prin1-to-string year) (prin1-to-string month))))
    (and str
         (let ((res (read-from-string str)))
           (if sort-predicate
               (sort res sort-predicate :key #'cdr)
               res)))))

(defun map-node-nums-before-time (function unix-time &optional (db *data-db*))
  (multiple-value-bind (y m) (decode-ymd unix-time)
    (dolist (year (get-years-before-year y db))
      (dolist (month (get-months-of-year year db))
        (when (or (<= year y) (<= month m))
          (dolist (info (get-month-post-info year month :db db))
            (when (or (null unix-time) (< (cdr info) unix-time))
              (funcall function (car info)))))))))

;; ((:link "post-name.html" :title "Post Name") ...)
(defun get-node-nums-before-time (count unix-time &optional (db *data-db*))
  (let ((node-nums nil)
        (cnt 0))
    (do-node-nums-before-time (node-num unix-time db)
      (push node-num node-nums)
      (incf cnt)
      (when (>= cnt count) (return)))
    (nreverse node-nums)))

(defun get-post-links-before-time (count unix-time &optional (db *data-db*))
  (loop for node-num in (get-node-nums-before-time count unix-time db)
     for node = (read-node node-num db)
     when node
     collect `(:link ,(car (getf node :aliases))
                     :title ,(getf node :title)
                     :node ,node-num)))

(defparameter *month-names*
  #("January"
    "February"
    "March"
    "April"
    "May"
    "June"
    "July"
    "August"
    "September"
    "October"
    "November"
    "December"))
(defun get-month-name (month)
  (aref *month-names* (1- month)))

(defparameter *day-names*
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defun get-day-name (day)
  (aref *day-names* day))

(defun month-link-and-name (year month)
  (values (format nil "ym-~d-~2,'0d.html" year month)
          (format nil "~a ~d" (get-month-name month) year)))
                
(defun get-previous-month (year month &optional (db *data-db*))
  (let ((years (cdr (member year (get-years-before-year year db))))
        (months (cdr (member month (get-months-of-year year db)))))
    (loop while (null months)
       do
         (setf year (pop years))
         (unless year
           (return-from get-previous-month nil))
         (setf months (get-months-of-year year db)))
    (values year (pop months))))

(defun get-next-month (year month &optional (db *data-db*))
  (let ((years (cdr (member year (get-years-after-year year db))))
        (months (cdr (member month (nreverse (get-months-of-year year db))))))
    (loop while (null months)
       do
         (setf year (pop years))
         (unless year
           (return-from get-next-month nil))
         (setf months (nreverse (get-months-of-year year db))))
    (values year (pop months))))

;; ((:link "ym-2001-03.html" :name "March 2011")
;;  (:link "ym-2001-04.html" :name "April 2011")
;;  (:link "ym-2001-05.html" :name "May 2011"))
(defun compute-month-link-plist (year month &optional (db *data-db*))
  (multiple-value-bind (py pm) (get-previous-month year month db)
    (multiple-value-bind (ny nm) (get-next-month year month db)
      (let ((res nil))
        (flet ((push-plist (y m)
                 (when y
                   (multiple-value-bind (link name) (month-link-and-name y m)
                     (push `(:link ,link :name ,name) res)))))
          (push-plist ny nm)
          (push-plist year month)
          (push-plist py pm)
          res)))))

(defun year-url (year)
  (format nil "y-~d.html" year))

;; (:months ((:link "2011-04.html" :name "April 2011") ...))
;;  :years ((:year (:link "y-2011.html" :name "2011")) ...))
(defun compute-months-and-years-link-plist (year month &optional (db *data-db*))
  (let ((years (get-years-before-year nil db)))
    `(,@(and month
             `(:sidebar-history-months
               ,(compute-month-link-plist year month db)))
      :sidebar-history-years
        ,(mapcar (lambda (y)
                   `(:link ,(year-url y)
                     :name ,y))
                 years))))

;; (:months ((:link "2011-04" :name "April 2011") ...))
;;  :years (2011 2010 2009 2008 2007 2006 2005 2004 2003 2002 2001)
;;  :recent-posts ((:link "post-name.html" :title "Post Name") ...)
;; )
(defun compute-history-plist (node &optional (db *data-db*))
  (with-settings (db)
    (when (integerp node)
      (setf node (data-get $NODES node :db db)))
    (let* ((time (getf node :created))
           (link-count (or (get-setting :previous-post-count)
                           10)))
      (when time
        (multiple-value-bind (y m) (decode-ymd time)
          (let ((posts (get-post-links-before-time link-count time db)))
            `(,@(compute-months-and-years-link-plist y m db)
                :recent-posts ,posts)))))))

(defun get-month-template (&optional (db *data-db*))
  (with-settings (db)
    (or (get-setting :month-template) ".month.tmpl")))

(defun get-year-template (&optional (db *data-db*))
  (with-settings (db)
    (or (get-setting :year-template) ".year.tmpl")))

(defun day-of-week (year month date &optional as-name-p)
  (let ((day (nth-value 6 (decode-universal-time
                           (encode-universal-time 0 0 0 date month year 0)
                           0))))
    (if as-name-p
        (get-day-name day)
        day)))

(defun get-day-alist (year month &optional (db *data-db*))
  (let ((infos (get-month-post-info
                year month :sort-predicate #'> :db db))
        (day-alist nil))
    (loop for info in infos
       do
         (multiple-value-bind (y m d) (decode-ymd (cdr info))
           (declare (ignore y m))
           (let ((cell (assoc d day-alist)))
             (if cell
                 (push info (cdr cell))
                 (push (list d info) day-alist)))))
    (sort day-alist #'< :key #'car)))

;; (:month "April 2011"
;;  :days ((:date-string "Thursday, 28 April"
;;          :posts
;;          ((:links ((:link <link> :title <title>)))))))
(defun compute-month-page-plist (year month &optional (db *data-db*))
  (let ((day-alist (get-day-alist year month db)))
    (loop for (date . infos) in day-alist
       for date-string = (format nil "~a, ~d ~d"
                                 (day-of-week year month date t)
                                 date
                                 (get-month-name month))
       collect `(:date-string ,date-string
                 :posts ,(mapcar
                          (lambda (info)
                            `(:links
                              ,(month-page-node-links (car info) db)))
                          infos))
       into days
       finally (return `(:month ,(format nil "~a ~d"
                                         (get-month-name month)
                                         year)
                         :link ,(month-link-and-name year month)
                         :days ,days)))))

(defun month-page-node-links (node-num &optional (db *data-db*))
  (let ((node (data-get $NODES node-num :db db)))
    (when node
      (let* ((link (car (getf node :aliases)))
             (title (getf node :title))
             (body (getf node :body))
             (links (extract-html-links body)))
        `((:link ,link :title ,title)
          ,@links)))))

;; ((:link "http://foo.com/bar.html" :title "Bar") ...)
(defun extract-html-links (html)
  (let ((res nil))
    (cl-ppcre:do-scans (ms me rs re
                           "<a .*?href=(?:'(.*?)'|\"(.*?)\").*?>(.*?)</a>"
                           html)
      (declare (ignore ms me))
      (let ((link (subseq html
                          (or (aref rs 0) (aref rs 1))
                          (or (aref re 0) (aref re 1))))
            (title (subseq html (aref rs 2) (aref re 2))))
        (push `(:link ,link :title ,title) res)))
    (nreverse res)))

(defun test-month-page-html (year month &optional (db *data-db*))
  (length
   (princ
    (render-template
     (get-month-template db)
     (compute-month-page-plist year month db)))))

;; (:months ((:month-string <date>
;;          :links ((:link <link> :title <title>)))))

(defun compute-year-page-plist (year &optional (db *data-db*))
  (loop
     with month-link
     with month-name
     for month in (nreverse (get-months-of-year year db))
     for day-links =
       (loop for infos in (get-month-post-info
                           year month :sort-predicate #'< :db db)
          for node-num = (car infos)
          for node = (data-get $NODES node-num :db db)
          for link = (car (getf node :aliases))
          for title = (getf node :title)
          when node
          collect `(:link ,link :title ,title))
     do (multiple-value-setq (month-link month-name)
          (month-link-and-name year month))
     collect `(:month-link ,month-link
               :month-name ,month-name
               :day-links ,day-links)
     into months
     finally (return `(:link ,year
                       :year ,year
                       :months ,months))))

(defun render-month-page (year month &key
                          (db *data-db*)
                          (site-db *site-db*))
  (multiple-value-bind (month-link month-name) (month-link-and-name year month)
    (let ((plist (compute-month-page-plist year month db)))
      (setf plist
            (append plist (compute-months-and-years-link-plist year month db)))
      (setf (getf plist :page-title) month-name)
      (setf (fsdb:db-get site-db month-link)
            (render-template (get-month-template db) plist :data-db db))
      month-link)))

(defun render-year-page (year &key (db *data-db*) (site-db *site-db*))
  (let ((plist (compute-year-page-plist year db))
        (year-string (princ-to-string year))
        (url (year-url year)))
    (setf plist
          (append plist (compute-months-and-years-link-plist year nil db)))
    (setf (getf plist :page-title) year-string)
    (setf (fsdb:db-get site-db url)
          (render-template (get-year-template db) plist :data-db db))
    url))

(defun update-node-year-and-month-pages (node &key
                                         (data-db *data-db*)
                                         (site-db *site-db*))
  (when (integerp node)
    (setf node (read-node node data-db)))
  (multiple-value-bind (y m)
      (update-node-year-index node data-db)
    (when y
      (render-month-page y m :db data-db :site-db site-db)
      (render-year-page y :db data-db :site-db site-db))))

(defun render-site (&key data-dir (db *data-db*) (site-db *site-db*) (verbose t)
                    (year-and-month-pages-only-p nil))
  (when data-dir
    (setf db (fsdb:make-fsdb data-dir))
    (with-site-db () (setf site-db *site-db*)))
  (let ((count 0))
    (macrolet ((rendering (form)
                 `(loop
                     (restart-case
                         (return (maybe-squawk ,form))
                       (retry-render ()
                         :report (lambda (stream)
                                   (format stream "Retry ~A." ',(car form))))))))
      (flet ((maybe-squawk (x)
               (when verbose
                 (format t "~&~s~%" x))))
        (dolist (year (get-years-before-year nil db))
          (rendering (render-year-page year :db db :site-db site-db))
          (incf count)
          (dolist (month (get-months-of-year year db))
            (rendering (render-month-page year month :db db :site-db site-db))
            (incf count)
            (unless year-and-month-pages-only-p
              (dolist (info (get-month-post-info year month :db db))
                (rendering (render-node (car info) :data-db db :site-db site-db))
                (incf count)))))
        (rendering (render-all-category-pages :data-db db :site-db site-db))
        (rendering (render-site-index :data-db db :site-db site-db))))
    count))

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
