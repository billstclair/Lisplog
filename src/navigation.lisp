; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Month and year pages, and last n posts
;;;

(in-package :lisplog)

(defun map-nodes (function &optional (db *data-db*))
  "Calls function with the plist for each node, in directory order"
  (flet ((get-node (path file)
           (let ((str (fsdb:db-get db path file)))
             (when str (read-from-string str)))))
    (declare (dynamic-extent #'get-node))
    (dolist (file (fsdb:db-contents db $NODES))
      (let ((path (fsdb:append-db-keys $NODES file)))
        (cond ((fsdb:db-dir-p db path)
               (dolist (file (fsdb:db-contents db path))
                 (funcall function (get-node path file))))
              (t (funcall function (get-node $NODES file))))))))

(defmacro do-nodes ((node &optional (db '*data-db*)) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block nil
       (flet ((,thunk (,node) ,@body))
         (declare (dynamic-extent #',thunk))
         (map-nodes #',thunk ,db)))))

;; Should do something to posts with status other than 1 here.
;; Then we'll be able to view them in the admin interface.
(defun decode-ym (unix-time)
  (multiple-value-bind (s min h d m y)
      (decode-universal-time (unix-to-universal-time unix-time))
    (declare (ignore s min h d))
    (values y m)))                             

(defun index-years (&optional (db *data-db*))
  (cl-fad:delete-directory-and-files
   (fsdb:db-filename db $YEARS) :if-does-not-exist :ignore)
  (let ((last-y 0))
    (do-nodes (node db)
      (when (eql 1 (getf node :status))
        (let* ((nid (getf node :nid))
               (created (getf node :created)))
          (multiple-value-bind (y m) (decode-ym created)
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
  (get-years-before-year (decode-ym unix-time) db))

(defun get-years-before-year (y &optional (db *data-db*))
  "Return a list of years up to Y that have posts"
  (sort
   (loop for year-str in (fsdb:db-contents db $YEARS)
      for year = (ignore-errors (parse-integer year-str))
      when (and year (<= year y))
      collect year)
   #'>))
         
(defun get-months-of-year (y &optional (db *data-db*))
  (sort
   (loop for month-str in (fsdb:db-contents db $YEARS (prin1-to-string y))
      for month = (ignore-errors (parse-integer month-str))
      when month
      collect month)
   #'>))

(defun get-month-post-info (year month &optional (db *data-db*))
  (let ((str (fsdb:db-get
              db $YEARS (prin1-to-string year) (prin1-to-string month))))
    (and str
         (sort (read-from-string str) #'> :key #'cdr))))

;; ((:link "post-name.html" :title "Post Name") ...)
(defun get-post-links-before-time (count unix-time &optional (db *data-db*))
  (let ((res nil)
        (cnt 0))
    (block outer
      (multiple-value-bind (y m) (decode-ym unix-time)
        (dolist (year (get-years-before-year y db))
          (dolist (month (get-months-of-year year db))
            (when (or (<= year y) (<= month m))
              (dolist (info (get-month-post-info year month db))
                (when (< (cdr info) unix-time)
                  (push info res)
                  (incf cnt)
                  (when (>= cnt count) (return-from outer)))))))))
    (loop for (node-num . time) in (nreverse res)
       for node = (and (< time unix-time) (data-get $NODES node-num))
       when node
       collect `(:link ,(car (getf node :aliases))
                 :title ,(getf node :title)
                 :node ,node-num))))

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

;; (:months ((:link "2011/04" :name "April 2011")))
;;  :years (2011 2010 2009 2008 2007 2006 2005 2004 2003 2002 2001)
;;  :recent-posts ((:link "post-name.html" :title "Post Name") ...)
;; )
(defun compute-navigation-plist (node-num &key (months 1) (link-count 5) (db *data-db*))
  (assert (eql months 1)
          nil
          "Haven't implemented months > 1")
  (let* ((node (data-get $NODES node-num :db db))
         (time (getf node :created)))
    (when time
      (multiple-value-bind (y m) (decode-ym time)
        (let ((month-link (format nil "~d/~2,'0d" y m))
              (month-name (format nil "~a ~d" (get-month-name m) y))
              (years (get-years-before-year y db))
              (posts (get-post-links-before-time link-count time)))
          `(:months ((:link ,month-link :name ,month-name))
            :years ,(mapcar (lambda (x) (list :year x)) years)
            :recent-posts ,posts))))))    

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
