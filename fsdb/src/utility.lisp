; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Various utility functions
;;;

(in-package :fsdb)

(defun file-get-contents (file)
  (with-open-file (stream file
                          :if-does-not-exist nil
                          :external-format :utf-8)
    (when stream
      (let* ((len (file-length stream))
             (s (make-string len)))
        (read-sequence s stream)
        s))))

(defun file-put-contents (file contents)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-sequence contents stream)
    contents))

(defparameter *whitespace* '(#\newline #\return #\tab #\space))

(defun trim (string)
  (string-left-trim *whitespace* (string-right-trim *whitespace* string)))

(defun assocequal (item alist)
  (assoc item alist :test 'equal))

(defun make-equal-hash (&rest keys-and-values)
  (let ((hash (make-hash-table :test 'equal)))
    (loop
       (when (null keys-and-values) (return))
       (let ((key (pop keys-and-values))
             (value (pop keys-and-values)))
         (setf (gethash key hash) value)))
    hash))

(defun get-inited-hash (key hash &optional (creator #'make-equal-hash))
  "Get an object from a hash table, creating it if it's not there."
  (or (gethash key hash)
      (setf (gethash key hash) (funcall creator))))

(defun strcat (&rest strings)
  "Concatenate a bunch of strings"
  (apply #'concatenate 'string (mapcar 'string strings)))

;; This should probably be smart enough to not eval PLACE twice,
;; but I only ever use it on symbols, so it doesn't really matter.
(defmacro dotcat (place &rest strings)
  `(setf ,place (strcat ,place ,@strings)))

(defun delq (elt list)
  (delete elt list :test #'eq))

(defun remove-trailing-separator (string &optional (separator #\/))
  (let ((len (length string)))
    (if (and (> len 0) (eql separator (aref string (1- len))))
        (subseq string 0 (1- len))
        string)))      

(defun implode (separator &rest strings)
  (declare (dynamic-extent strings))
  (let ((res (if (null strings) "" (car strings))))
    (dolist (item (cdr strings))
      (setq res (strcat res separator item)))
    res))

(defun explode (separator string)
  (when (stringp separator)
    (assert (eql (length separator) 1))
    (setq separator (elt separator 0)))
  (check-type separator character)
  (let* ((len (length string))
         (res
          (loop
             with len = (length string)
             for start = 0 then (1+ end)
             while (< start len)
             for end = (or (position separator string :start start) len)
             collect (subseq string start end))))
    (if (and (> len 0) (eql separator (aref string (1- len))))
        (nconc res (list ""))
        res)))

(defun strstr (haystack needle)
  "Find NEEDLE in HAYSTACK. Return the tail of HAYSTACK including NEEDLE."
  (let ((pos (search needle haystack)))
    (and pos (subseq haystack pos))))

(defun str-replace (old new string)
  "Change all instance of OLD to NEW in STRING"
  (loop
     with oldstr = (string old)
     with newstr = (string new)
     with pos = 0
     with old-len = (length oldstr)
     with new-len = (length newstr)
     with res = string
     for idx = (search oldstr res :start2 pos)
     do
       (when (null idx) (return res))
       (setq res (strcat (subseq res 0 idx) newstr (subseq res (+ idx old-len)))
             pos (+ idx new-len))))

(defun blankp (x)
  (or (null x) (equal x "")))

(defun stringify (x &optional format)
  (format nil (or format "~a") x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2010 Bill St. Clair
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
