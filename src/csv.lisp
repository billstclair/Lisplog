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
         (funcall line-function (nreverse line)))
       (when in-quote (error "Missing closing quote"))))

(defun count-csv-lines (file)
  (let ((cnt 0))
    (flet ((f (line)
             (declare (ignore line))
             (incf cnt)))
      (parse-csv file #'f))
    cnt))

(defun node-read (db path)
  (let ((str (fsdb:db-get db path)))
    (and str (read-from-string str))))

(defun (setf node-read) (alist db path)
  (setf (fsdb:db-get db path) (with-output-to-string (s) (pprint alist s))))

(defun parse-drupal-csv (file fsdb path required-key line-function)
  (let ((keys nil)
        (first-p t)
        (db (fsdb:db-subdir fsdb path)))
    (flet ((line-function (line)
             (cond (first-p
                    (setf first-p nil)
                    (setf keys
                          (mapcar (lambda (x)
                                    (intern (string-upcase x) :keyword))
                                  line))
                    (unless (memq required-key keys)
                      (error "No ~s key" required-key)))
                   (t (funcall line-function line db keys)))))
      (parse-csv file #'line-function))))

(defun parse-drupal-node-line (line fsdb keys)
  (let* ((vals (mapcar 'cons keys line))
         (nid (cdr (assq :nid vals)))
         (teaser-cell (assq :teaser vals))
         (created-cell (assq :created vals))
         (changed-cell (assq :changed vals)))
    (dolist (cell vals)
      (awhen (ignore-errors (parse-integer (cdr cell)))
        (setf (cdr cell) it)))
    (when nid
      (when (equal (cdr teaser-cell)
                   (cdr (assq :body vals)))
        (setf (cdr teaser-cell) nil))
      (flet ((convert (unix-time)
               (if (integerp unix-time)
                   (unix-to-universal-time unix-time)
                   unix-time)))
        (when created-cell
          (setf (cdr created-cell) (convert (cdr created-cell))))
        (when changed-cell
          (setf (cdr changed-cell) (convert (cdr changed-cell)))))
      (let* ((file (format nil "~a.txt" nid)))
        (setf (node-read fsdb file) vals)
        nil))))

;; (set' db (fsdb:make-fsdb "~/etwof/blog.new/data"))
;; (parse-drupal-node-csv "node.csv" db "nodes")
(defun parse-drupal-node-csv (file fsdb path)
  "Parse the NODE table from Drupal into a file per node in the FSDB."
  (parse-drupal-csv file fsdb path :nid 'parse-drupal-node-line))

(defun parse-drupal-url-alias-line (line db keys)
  (let* ((vals (mapcar 'cons keys line))
         (src (cdr (assq :src vals)))
         (dest (cdr (assq :dest vals)))
         (node-p (and (stringp src) (eql 0 (search "node/" src)))))
    (when node-p
      (let* ((node (subseq src 5))
             (key (strcat node ".txt"))
             (alist (node-read db key)))
        (pushnew dest (assqv :aliases alist) :test #'equal)
        (format t "~s => ~s~%" node dest)
        (setf (node-read db key) alist)))))

;; Run parse-drupal-node-csv first
;; Add url aliases to node files.
;; (parse-drupal-url-alias-csv "url_alias.csv" db "nodes")
(defun parse-drupal-url-alias-csv (file fsdb path)
  (parse-drupal-csv file fsdb path :src 'parse-drupal-url-alias-line))

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
