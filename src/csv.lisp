; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for the comma-separate-value (csv) files dumped by PHPMyAdmin
;;;

(in-package :lisplog)

(defvar *csv-delimiter* #\")
(defvar *csv-separator* #\;)
(defvar *csv-escape* #\\)
(defvar *csv-ignore-characters* '(#\return))

(defun parse-csv (file &optional line-function)
  (let* ((res nil)
         (line-function (or line-function (lambda (x) (push x res)))))
    (with-open-file (s file :external-format :utf-8)
      (parse-csv-stream s line-function))
    (nreverse res)))

(defun parse-csv-stream (s line-function)
  (loop with escaped = nil
     with in-quote = nil
     with last-fun = nil
     with outs = (make-string-output-stream)
     with line = nil
     for char = (read-char s nil :eof)
     for new-last-fun = nil
     until (eq char :eof)
     do
       (cond ((member char *csv-ignore-characters*))
             (escaped (write-char char outs)
                      (setf escaped nil))
             ((eql char *csv-delimiter*)
              (cond (in-quote
                     (push (get-output-stream-string outs) line)
                     (setf in-quote nil))
                    (t (setf in-quote t))))
             ((not in-quote)
              (cond ((eql char #\newline)
                     (when (eq last-fun :delim)
                       (push "" line))
                     (setf new-last-fun :newline)
                     (funcall line-function (nreverse line))
                     (setf line nil))
                    ((eql char *csv-separator*)
                     (setf new-last-fun :delim)
                     (when (eq last-fun :delim)
                       (push "" line)))
                    (t (error "Missing beginning delimiter"))))
             ((eql char *csv-escape*)
              (setf escaped t))
             (t (write-char char outs)))
       (setf last-fun new-last-fun)
     finally
       (when in-quote (error "File ended with open quote"))
       (unless (eq last-fun :newline)
         (when (eq last-fun :delim) (push "" line))
         (funcall line-function (nreverse line)))
       (when in-quote (error "Missing closing quote"))))

(defun count-csv-lines (file)
  (let ((cnt 0))
    (flet ((f (line)
             (declare (ignore line))
             (incf cnt)))
      (parse-csv file #'f))
    cnt))

(defparameter *csv-dir* (merge-pathnames "csv/" *lisplog-home*))

(defun csv-file (file)
  (merge-pathnames file *csv-dir*))

(defun parse-drupal-csv (file db required-key line-function &rest rest)
  (let ((keys nil)
        (first-p t))
    (flet ((line-function (line)
             (cond (first-p
                    (setf first-p nil)
                    (setf keys
                          (mapcar (lambda (x)
                                    (intern (string-upcase x) :keyword))
                                  line))
                    (unless (memq required-key keys)
                      (error "No ~s key" required-key)))
                   (t
                    (let ((plist (make-plist keys line)))
                      (loop for tail on (cdr plist) by #'cddr
                         for val = (ignore-errors (parse-integer (car tail)))
                         when val do
                          (setf (car tail) val))
                      (apply line-function plist db rest))))))
      (parse-csv (csv-file file) #'line-function))))

;;;
;;; Parsers for individual Drupal table dumps.
;;; Export from the "Export" tab on a table's page in PHPMyAdmin
;;; Choose "CSV" in the "Export" box
;;; Change "Replace NULL by" from "NULL" to blank
;;; Check "Put field names in the first row"
;;; Click the "Go" button.
;;; Move the file from wherever the browser saves it to lisplog/csv/
;;;
;;; Drupal table  "data" subdir
;;; ============  =============
;;; node          $NODES
;;; url_alias     $NODES (the :aliases property of each)
;;; comments      $COMMENTS (+ each node's :comments property)
;;; users         $USERS
;;; interwiki     $INTERWIKI
;;; term_data     $CATEGORIES
;;; term_node     $CATNODES (+ each node's :categories & :cat-neighbors property)
;;;

(defun store-drupal-node-line (plist db)
  (let* ((nid (getf plist :nid)))
    (when nid
      (when (equal (getf plist :teaser) (getf plist :body))
        (setf (getf plist :teaser) nil))
      (setf (node-get db $NODES nid) plist)
      nil)))

;; (set' db (fsdb:make-fsdb "~/lisplog/data"))
;; (parse-drupal-node-csv db "node.csv")
(defun parse-drupal-node-csv (db &optional (file "node.csv"))
  "Parse the NODE table from Drupal into a file per node in the FSDB."
  (parse-drupal-csv file db :nid 'store-drupal-node-line))

(defun store-drupal-url-alias-line (plist db &optional verbose)
  (let* ((src (getf plist :src))
         (dest (getf plist :dest))
         (node-p (and (stringp src) (eql 0 (search "node/" src)))))
    (when node-p
      (let* ((node (subseq src 5)))
        (updating-node (plist db $NODES node)
          (when verbose
            (format t "~s => ~s~%" node dest))
          (pushnew dest (getf plist :aliases) :test #'equal))))))

;; Run parse-drupal-node-csv first
;; Add url aliases to node files.
;; (parse-drupal-url-alias-csv db "url_alias.csv")
(defun parse-drupal-url-alias-csv (db &optional file verbose)
  (unless file
    (setf file  "url_alias.csv"))
  (parse-drupal-csv file db :src 'store-drupal-url-alias-line verbose))

(defun store-drupal-comments-line (plist db &optional verbose)
  (let ((cid (getf plist :cid))
        (nid (getf plist :nid)))
    (when verbose
      (format t "Comment: ~s for node: ~s~%" cid nid))
    (updating-node (node db $NODES nid)
      (pushnew cid (getf node :comments)))
    (setf (node-get db $COMMENTS cid) plist)))

(defun parse-drupal-comments-csv (db &optional file verbose)
  (unless file
    (setf file "comments.csv"))
  (parse-drupal-csv file db :cid 'store-drupal-comments-line verbose))

(defun store-drupal-users-line (plist db &optional verbose)
  (let ((uid (getf plist :uid))
        (status (getf plist :status)))
    (unless (zerop status)
      (when verbose
        (let ((name (getf plist :name))
              (mail (getf plist :mail)))
          (format t "~s ~s ~s~%" uid name mail)))
      (setf (node-get db $USERS uid) plist))))

;; Remember to "Replace NULL by" blank, not "NULL", in the
;; phpMyAdmin export form.
(defun parse-drupal-users-csv (db &optional file verbose)
  (unless file
    (setf file "users.csv"))
  (parse-drupal-csv file db :uid 'store-drupal-users-line verbose))

(defun store-drupal-interwiki-line (plist db &optional verbose-p)
  (let ((key (getf plist :iw_prefix)))
    (when verbose-p (print plist))
    (unless (blankp key)
      (setf (node-get db $INTERWIKI key :subdirs-p nil) plist))))

(defun parse-drupal-interwiki-csv (db &optional file verbose-p)
  (unless file
    (setf file "interwiki.csv"))
  (parse-drupal-csv file db :iw_prefix 'store-drupal-interwiki-line verbose-p))

(defun store-drupal-category-csv (plist db &optional verbose-p)
  (let ((key (getf plist :tid))
        (vid (getf plist :vid)))
    (when (eql vid 3)
      (when verbose-p (print plist))
      (unless (blankp key)
        (setf (node-get db $CATEGORIES key :subdirs-p nil) plist)))))

(defun parse-drupal-category-csv (db &optional file verbose-p)
  (unless file
    (setf file "term_data.csv"))
  (parse-drupal-csv file db :tid 'store-drupal-category-csv verbose-p))

;; Most of this really belongs somewhere else as a function
;; to run on the database.
(defun parse-drupal-term-node-csv (&optional db file)
  (unless db
    (setf db *data-db*))
  (unless file
    (setf file "term_node.csv"))
  (let ((term-hash (make-hash-table :size 5000))
        (term-node-plist nil))
    (flet ((add-node (plist db)
             (declare (ignore db))
             (setf (gethash (getf plist :nid) term-hash) (getf plist :tid))))
      (parse-drupal-csv file db :tid #'add-node))
    (maphash (lambda (nid tid)
               (let ((time (getf (data-get $NODES nid :db db) :created)))
                 (when time
                   (push (cons nid time) (getf term-node-plist tid)))))
             term-hash)
    (loop for (tid nodes) on term-node-plist by #'cddr
       do
         (setf nodes (sort nodes '> :key #'cdr)
               (data-get $CATNODES tid :db db :subdirs-p nil) nodes)
         (loop with last = (last nodes)
            ;; List is sorted with newer entries first
            for next = (caar last) then node-num
            for node-num = (caar nodes) then prev
            for node = (data-get $NODES node-num :db db) then prev-node
            for prev-cell in (progn (setf (cdr last) (list (car nodes)))
                               (cdr nodes))
            for prev = (car prev-cell)
            for prev-node = (data-get $NODES prev)
            for neighbors = (getf node :cat-neighbors)
            do
              (cond ((eql 0 (getf prev-node :status))
                     ;; Unshown node, skip it
                     (setf prev node-num
                           prev-node node
                           node-num next))
                    (t
                     (setf (getf neighbors tid) (cons prev next)
                           (getf node :cat-neighbors) neighbors)
                     (setf (data-get $NODES node-num :db db) node)))))))

;;;
;;; Fixup functions
;;;

(defun fix-badly-named-posts (&key (db *data-db*) verbose-p)
  (do-nodes (node db)
    (let* ((alias (car (getf node :aliases)))
           (pos (search ".html_" alias :test #'equal)))
      (when pos
        (setf alias (strcat (subseq alias 0 pos)
                            (subseq alias (+ pos 5))
                            ".html"))
        (when verbose-p
          (format t "~&~d: ~s~%" (getf node :nid) alias))
        (setf (car (getf node :aliases)) alias)
        (setf (data-get $NODES (getf node :nid) :db db) node)))))

(defun find-non-html-posts (&optional (db *data-db*))
  (let (res)
    (do-nodes (node db)
      (let ((aliases (getf node :aliases)))
        (dolist (alias aliases)
          (unless (eql (search ".html" alias :from-end t :test #'string-equal)
                       (- (length alias) 5))
            (push (cons (getf node :nid) aliases) res)
            (return)))))
    (nreverse res)))

(defun fix-nodes (fixer &key comments-p (db *data-db*))
  (let ((cnt 0))
    (do-nodes (node db)
      (when (or (funcall fixer node :teaser)
                (funcall fixer node :body))
        (setf (data-get $NODES (getf node :nid) :db db) node)
        (incf cnt)))
    (when comments-p
      (do-comments (comment db)
        (when (funcall fixer comment :comment)
          (setf (data-get $COMMENTS (getf comment :cid)) comment)
          (incf cnt))))
    cnt))  

(defparameter *bad-char-plist*
  `(,(concatenate
      'string '(#\Latin_Small_Letter_A_With_Circumflex #\U+20AC #\U+201D))
     "--"
    ))

(defun fix-bad-plist-entry (plist key)
  (let ((entry (getf plist key))
        (res nil))
    (when (stringp entry)
      (loop for (search replace) on *bad-char-plist* by #'cddr
         when (search search entry :test #'equal)
         do
           (setf res t)
           (setf (getf plist key) (fsdb:str-replace search replace entry))))
    res))

(defun fix-bad-chars (&optional (db *data-db*))
  (fix-nodes #'fix-bad-plist-entry :comments-p t :db db))

(defun fix-slashdot-entry (plist key)
  (let ((text (getf plist key))
        (res nil)
        (search-string "[/.:"))
    (when (search search-string text)
      (setf (getf plist key) (fsdb:str-replace search-string "[slashdot:" text))
      (setf res t))
    res))

(defun fix-slashdot (&optional (db *data-db*))
  (fix-nodes #'fix-slashdot-entry :comments-p t :db db))

;; This gets enough to make the main site.
;; Still need to add the categories and the aggregator,
;; but I'm going to start with this.
(defun parse-main-drupal-files (&key
                                (dir "~/lisplog/data")
                                (db (fsdb:make-fsdb dir)))
  (format t "~&Parsing nodes...")
  (parse-drupal-node-csv db)
  (format t " Done.~%Parsing URL aliases...")
  (parse-drupal-url-alias-csv db)
  (format t " Done.~%Parsing comments...")
  (parse-drupal-comments-csv db)
  (format t " Done.~%Parsing users...")
  (parse-drupal-users-csv db)
  (format t " Done.~%Parsing categories...")
  (parse-drupal-category-csv db)
  (format t " Done.~%Parsing node to category mapping...")
  (parse-drupal-term-node-csv db)
  (format t " Done.~%Renaming quote posts...")
  (fix-badly-named-posts :db db)
  (format t " Done.~%Fixing bad chars...")
  (fix-bad-chars db)
  (format t " Done.~%Fixing slashdot credits...")
  (fix-slashdot db)
  (format t " Done.~%Indexing years...")
  (index-years db)
  (format t " Done.~%Indexing users...")
  (index-users db)
  (format t " Done.~%")
  db)

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
