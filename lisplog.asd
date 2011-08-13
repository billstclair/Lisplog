; -*- mode: lisp -*-
(in-package #:cl-user)

(asdf:defsystem :lisplog
  :description "Simple blogging in Common Lisp"
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "0.01"
  :license "Apache"
  :depends-on (fsdb cl-crypto           ;local
               md5 anaphora html-template cl-fad cl-ppcre
               split-sequence bordeaux-threads hunchentoot
               limited-thread-taskmaster cl-base64 cl-smtp)
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
     (:file "sessions")
     (:file "captcha")
     (:file "webserver")
     ))))

(unless (or (find-package :cl-autorepo)
            (ignore-errors (ql:quickload "cl-autorepo")))
  (let* ((dir "~/.local/share/common-lisp/source/")
         (autorepo-asd (merge-pathnames "cl-autorepo/cl-autorepo.asd" dir))
         (url "https://github.com/billstclair/cl-autorepo"))
    (asdf:run-shell-command "mkdir -p ~a;cd ~a;git clone ~a" dir dir url)
    (load autorepo-asd)
    (ql:quickload "cl-autorepo")))

(cl-autorepo:add-system "fsdb" "git://github.com/billstclair/fsdb.git" :git)
(cl-autorepo:add-system
 "limited-thread-taskmaster"
 "git://github.com/billstclair/limited-thread-taskmaster.git"
 :git)
(cl-autorepo:add-system
 "cl-crypto" "git://github.com/billstclair/cl-crypto.git" :git)

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
