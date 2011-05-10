; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Webserver sessions
;;;

(in-package :lisplog)

(defclass session ()
  ((session-id :reader session-id-of :initarg :session-id)
   (uid :accessor uid-of :initarg :uid)
   (timestamp :accessor timestamp-of :initarg :timestamp
              :initform (get-universal-time))
   (plist :accessor plist-of :initarg :plist)))

(defmethod initialize-instance :after ((session session) &key &allow-other-keys)
  (let ((plist (plist-of session)))
    (setf (getf plist :session-id) (session-id-of session)
          (plist-of session) plist)))

(defmethod print-object ((session session) stream)
  (print-unreadable-object (session stream :type t)
    (format stream "~s ~s" (session-id-of session) (uid-of session))))

(defun session-get (session property)
  (getf (plist-of session) property))

(defun (setf session-get) (value session property)
  (assert (not (eq property :session-id)))
  (setf (getf (plist-of session) property) value))

(defmethod (setf uid-of) :after (uid (session session))
  (setf (session-get session :uid) uid))

(defmethod (setf timestamp-of) :after (timestamp (session session))
  (setf (session-get session :timestamp) timestamp))

(defun new-session-id ()
  (let ((x (cl-crypto:get-ranged-random-num (expt 2 127) (expt 2 128))))
    (string-downcase (format nil "~x" x))))

(defun read-session (session-id &optional (db *data-db*))
  (let ((plist (data-get $SESSIONS session-id :db db)))
    (when plist
      (make-instance 'session
                     :session-id session-id
                     :uid (getf plist :uid)
                     :timestamp (getf plist :timestamp)
                     :plist plist))))

(defun write-session (session &optional (db *data-db*))
  (check-type session session)
  (let* ((plist (plist-of session))
         (session-id (session-id-of session))
         (uid (uid-of session))
         (user (and uid (read-user uid db)))
         (sessions (getf user :sessions)))
    (when user
      (setf sessions
            (cons session-id (delete session-id sessions :test #'equal)))
      ;; Allow a user to have 5 open sessions
      (when (> (length sessions) 5)
        (let ((tail (nthcdr 4 sessions)))
          (psetf tail (cdr tail)
                 (cdr tail) nil)
          (dolist (id tail)
            (setf (data-get $SESSIONS id) nil))))
      (setf (getf user :sessions) sessions
            (read-user uid db) user)
      (setf (getf plist :uid) (uid-of session)
            (getf plist :timestamp) (timestamp-of session)
            (data-get $SESSIONS (session-id-of session) :db db) plist))))
          
(defvar *session-hash* (make-hash-table :test 'equal))

(defvar *session-lock*
  (bt:make-recursive-lock "*session-lock*"))

(defmacro with-session-lock (&body body)
  `(bt:with-recursive-lock-held (*session-lock*)
     ,@body))

(defun make-session (&key
                     (session-id (new-session-id))
                     (uid nil)
                     (timestamp (get-universal-time))
                     (plist nil))
  (with-session-lock
    (assert (not (gethash session-id *session-hash*)))
    (setf (gethash session-id *session-hash*)
          (make-instance 'session
                         :session-id session-id
                         :uid uid
                         :timestamp timestamp
                         :plist plist))))
                     
(defun get-session (session-id &optional (db *data-db*))
  (with-session-lock
    (or (gethash session-id *session-hash*)
        (let ((session (read-session session-id db)))
          (when session
            (setf (gethash session-id *session-hash*) session))))))

;;;
;;; Hunchentoot interface
;;;

;; This allows serving multiple weblogs from a single lisp image.
;; That will work if they take data from separate data directories,
;; and render to separate site directories.
;; It may fail in mysterious ways if you try to
;; share a data or site directory between two ports.

(defvar *port-db-alist* nil)
(defvar *port-acceptor-alist* nil)

(defun get-port-db (&optional (port (hunchentoot:acceptor-port
                                     hunchentoot:*acceptor*)))
  (cdr (assoc port *port-db-alist*)))

(defun (setf get-port-db) (db &optional (port (hunchentoot:acceptor-port
                                               hunchentoot:*acceptor*)))
  (let ((cell (assoc port *port-db-alist*)))
    (if cell
        (setf (cdr cell) db)
        (push (cons port db) *port-db-alist*))))

(defun get-port-acceptor (&optional (port (hunchentoot:acceptor-port
                                           hunchentoot:*acceptor*)))
  (cdr (assoc port *port-acceptor-alist*)))

(defun (setf get-port-acceptor) (acceptor &optional
                                 (port (hunchentoot:acceptor-port
                                        hunchentoot:*acceptor*)))
  (let ((cell (assoc port *port-acceptor-alist*)))
    (if cell
        (setf (cdr cell) acceptor)
        (push (cons port acceptor) *port-acceptor-alist*)))
  acceptor)

(defclass lisplog-request (hunchentoot:request)
  ())

(defclass lisplog-acceptor (hunchentoot:acceptor)
  ()
  (:default-initargs :request-class 'lisplog-request))

(defun start-session ()
  (or (hunchentoot:session hunchentoot:*request*)
      (let ((session (make-session)))
        (hunchentoot:set-cookie
         (hunchentoot:session-cookie-name hunchentoot:*acceptor*)
         :value (hunchentoot:session-cookie-value session)
         :path "/"
         :expires (encode-universal-time 0 0 0 1 1 3000))
        (setf (hunchentoot:session hunchentoot:*request*) session
              hunchentoot:*session* session))))

(defmethod hunchentoot:session-cookie-value ((session session))
  (session-id-of session))

(defmethod hunchentoot:session-cookie-name ((acceptor lisplog-acceptor))
  (with-settings ((get-port-db))
    (or (get-setting :session-cookie-name)
        "lisplog-session")))

(defmethod hunchentoot:session-verify ((request lisplog-request))
  (let* ((cookie-name (hunchentoot:session-cookie-name hunchentoot:*acceptor*))
         (session-id (or (hunchentoot:cookie-in cookie-name request)
                        (hunchentoot:get-parameter cookie-name request))))
    (and session-id (get-session session-id (get-port-db)))))


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
