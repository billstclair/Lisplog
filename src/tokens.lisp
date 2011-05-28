; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tokens, to help prevent typos
;;;

(in-package :lisplog)

(defmacro tokens (&rest syms)
  `(progn
     ,@(loop for sym in syms
          for name = (symbol-name sym)
          for str = (string-downcase (subseq name 1))
          collect `(defconstant ,sym ,str)
          do (assert (eql #\$ (elt name 0))))))

(tokens $CONFIG
        $COUNTERS
        $NODES
        $COMMENTS
        $USERS
        $MODERATION
        $STYLES
        $BLOCKS
        $SETTINGS
        $DATA
        $SITE
        $YEARS
        $INTERWIKI
        $CATEGORIES
        $CATNODES
        $USERNAMEHASH
        $SESSIONS
        $DEFAULT
        $COUNTERS
        $NID
        $CID
        $CAPTCHA
        $MODERATION
        )

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
