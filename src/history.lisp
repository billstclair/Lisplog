; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Month and year pages, and last n posts
;;;

(in-package :lisplog)

(defun map-nodes-or-comments (function nodes-or-comments &optional (db *data-db*))
  "Calls function with the plist for each node, in directory order"
  (flet ((get-node (path file)
           (let ((str (fsdb:db-get db path file)))
             (when str (read-from-string str)))))
    (declare (dynamic-extent #'get-node))
    (dolist (file (fsdb:db-contents db nodes-or-comments))
      (let ((path (fsdb:append-db-keys nodes-or-comments file)))
        (cond ((fsdb:db-dir-p db path)
               (dolist (file (fsdb:db-contents db path))
                 (funcall function (get-node path file))))
              (t (funcall function (get-node nodes-or-comments file))))))))

(defun map-nodes (function &optional (db *data-db*))
  (map-nodes-or-comments function $NODES db))

(defmacro do-nodes ((node &optional (db '*data-db*)) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block nil
       (flet ((,thunk (,node) ,@body))
         (declare (dynamic-extent #',thunk))
         (map-nodes #',thunk ,db)))))

(defun map-comments (function &optional (db *data-db*))
  (map-nodes-or-comments function $COMMENTS db))

(defmacro do-comments ((comment &optional (db '*data-db*)) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block nil
       (flet ((,thunk (,comment) ,@body))
         (declare (dynamic-extent #',thunk))
         (map-comments #',thunk ,db)))))

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

;; ((:link "post-name.html" :title "Post Name") ...)
(defun get-node-nums-before-time (count unix-time &optional (db *data-db*))
  (let ((node-nums nil)
        (cnt 0))
    (block outer
      (multiple-value-bind (y m) (decode-ymd unix-time)
        (dolist (year (get-years-before-year y db))
          (dolist (month (get-months-of-year year db))
            (when (or (<= year y) (<= month m))
              (dolist (info (get-month-post-info year month :db db))
                (when (or (null unix-time) (< (cdr info) unix-time))
                  (push (car info) node-nums)
                  (incf cnt)
                  (when (>= cnt count) (return-from outer)))))))))
    (nreverse node-nums)))

(defun get-post-links-before-time (count unix-time &optional (db *data-db*))
  (loop for node-num in (get-node-nums-before-time count unix-time db)
     for node = (data-get $NODES node-num)
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
          (push-plist py pm))))))

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
(defun compute-history-plist (node-num &optional (db *data-db*))
  (with-settings (db)
    (let* ((node (data-get $NODES node-num :db db))
           (time (getf node :created))
           (link-count (or (get-setting :previous-post-count)
                           10)))
      (when time
        (multiple-value-bind (y m) (decode-ymd time)
          (let ((posts (get-post-links-before-time link-count time)))
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
                year month :sort-predicate #'< :db db))
        (day-alist nil))
    (loop for info in infos
       do
         (multiple-value-bind (y m d) (decode-ymd (cdr info))
           (declare (ignore y m))
           (push info (assqv d day-alist))))
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
            (render-template (get-month-template db) plist))
      month-link)))

(defun render-year-page (year &key (db *data-db*) (site-db *site-db*))
  (let ((plist (compute-year-page-plist year db))
        (year-string (princ-to-string year))
        (url (year-url year)))
    (setf plist
          (append plist (compute-months-and-years-link-plist year nil db)))
    (setf (getf plist :page-title) year-string)
    (setf (fsdb:db-get site-db url)
          (render-template (get-year-template db) plist))
    url))

(defun render-site (&key (db *data-db*) (site-db *site-db*) (verbose t))
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
            (dolist (info (get-month-post-info year month :db db))
              (rendering (render-node (car info) :data-db db :site-db site-db))
              (incf count))))
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
