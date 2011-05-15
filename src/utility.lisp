; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; <Description>
;;;

(in-package :lisplog)

(defun memq (x list)
  (loop for tail on list
     when (eq x (car tail))
     return tail))

(defun delq (x list)
  (loop for pre-tail = nil then tail
     for tail on list do
       (when (eq (car tail) x)
         (if (eq tail list)
             (setf list (cdr tail))
             (setf (cdr pre-tail) (cdr tail)))))
  list)

(defun assq (x alist)
  (dolist (cell alist)
    (when (eq x (car cell))
      (return cell))))

(defun assqv (key alist)
  (cdr (assq key alist)))

(defun ensure-list (x)
  (if (listp x) x (list x)))

;; This doesn't work right. Don't use it.
#+not
(define-setf-expander assqv (key alist &environment env)
  (multiple-value-bind (dummies vals stores setter getter)
      (get-setf-expansion alist env)
    (let ((store (gensym "STORE"))
          (cellv (gensym "CELL"))
          (alistv (gensym "ALIST"))
          (keyv (gensym "KEY"))
          (stemp (car stores)))
      (values 
       (list* alistv keyv cellv  dummies)
       (list* getter key `(assq ,keyv ,alistv) vals)
       `(,store)
       `(let ((,stemp ,alistv))
          (unless ,cellv
            (setf ,cellv (cons ,keyv ,store))
            (push ,cellv ,stemp))
          ,setter
          ,store)
       `(cdr ,cellv)))))

(defun strcat (&rest strings)
  (declare (dynamic-extent strings))
  (apply #'concatenate 'string strings))

(defun blankp (x)
  (or (null x) (equal x "")))

(defun make-plist (keys values)
  (assert (eql (length keys) (length values)))
  (mapcan 'list keys values))

(defun md5 (string)
  (let ((digest (md5:md5sum-sequence string)))
    (format nil "~(~{~2,'0X~}~)" (map 'list #'identity digest))))


;;;
;;; Hunchentoot
;;;

(defun hsc (x)
  (and x (hunchentoot:escape-for-html x)))

(defun parm (name &rest args)
  (hunchentoot:parameter
   (if args (apply #'format nil name args) name)))

(defun parms (&key (post t) (get nil))
  (let ((req hunchentoot:*request*))
    (append (and post (hunchentoot:post-parameters req))
            (and get (hunchentoot:get-parameters req)))))

(defun post-parm (name &rest args)
  (hunchentoot:post-parameter
   (if args (apply #'format nil name args) name)))

(defun get-host-name ()
  (usocket::get-host-name))

(defvar *startup-functions* nil)

(defun add-startup-function (function)
  (pushnew function *startup-functions*))

(defun run-startup-functions ()
  (dolist (function (reverse *startup-functions*))
    (funcall function)))

(defun xor (&rest args)
  "True if an even number of args are true"
  (let ((res nil))
    (dolist (arg args)
      (when arg (setq res (not res))))
    res))

(defun xor-strings (s1 s2)
  (with-output-to-string (s)
    (let ((s1-len (length s1))
          (s2-len (length s2))
          s-tail s-tail-offset s-tail-len)
      (dotimes (i (min s1-len s2-len))
        (write-char
         (code-char
          (logxor (char-code (aref s1 i)) (char-code (aref s2 i))))
         s))
      (if (> s1-len s2-len)
          (setq s-tail s1
                s-tail-offset s2-len
                s-tail-len (- s1-len s2-len))
          (setq s-tail s2
                s-tail-offset s1-len
                s-tail-len (- s2-len s1-len)))
      (dotimes (i s-tail-len)
        (write-char (aref s-tail (+ s-tail-offset i)) s)))))

(defun browse-url (url)
  (declare (ignorable url))
  #+darwin
  (run-program "open" (list url))
  #+windows
  (run-program
   "/windows/system32/rundll32"
   (list "url.dll,FileProtocolHandler" (format nil "\"~a\"" url)))
  #+linux
  (run-program "firefox" (list url))    ; support only Firefox for now
  )

;;;
;;; Unix time
;;;

(defparameter *time-offset*
  (- (encode-universal-time 0 0 0 1 1 1970)
     (encode-universal-time 0 0 0 1 1 1900)))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *time-offset*))

(defun universal-to-unix-time (universal-time)
  (- universal-time *time-offset*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))



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
