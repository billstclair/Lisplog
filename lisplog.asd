; -*- mode: lisp -*-
(in-package #:cl-user)

(defparameter *lisplog-home*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(pushnew (merge-pathnames "fsdb/" *lisplog-home*)
         asdf:*central-registry*
         :test #'equal)

(asdf:defsystem :lisplog
  :description "Simple blogging in Common Lisp"
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "0.01"
  :license "Apache"
  :depends-on (fsdb md5 anaphora html-template cl-fad cl-ppcre hunchentoot)
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     #+ccl
     (:file "ccl")               ;dev tools, not needed in other lisps
     (:file "utility")
     (:file "tokens")
     (:file "db")
     (:file "templates")
     (:file "history")
     (:file "csv")
     ))))

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
