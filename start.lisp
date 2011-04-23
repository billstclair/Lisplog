(in-package :cl-user)

;; One-time only
;(load "http://beta.quicklisp.org/quicklisp.lisp")
;(quicklisp-quickstart:install)

(unless (find-package "QUICKLISP")
  (load "~/quicklisp/setup"))

(ql:quickload "lisplog")

;; Can't do this inline since *package* is bound by LOAD
(defun set-package ()
  (swank:set-package :lisplog))
