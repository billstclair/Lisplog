(in-package :cl-user)

;; One-time only
;(load "http://beta.quicklisp.org/quicklisp.lisp")
;(quicklisp-quickstart:install)

(unless (find-package "QUICKLISP")
  (load "~/quicklisp/setup"))

(ql:quickload "lisplog")
(ql:quickload "swank")

(defun ll ()
  (swank:set-package :lisplog))

(ll)

(defun start-swank (&optional port)
  (when port
    (swank:create-server :port port :dont-close t)))

(defun reload ()
  (ql:quickload "lisplog"))
