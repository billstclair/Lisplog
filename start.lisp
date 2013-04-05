(in-package :cl-user)

(unless (find-package "QUICKLISP")
  (cond ((probe-file "~/quicklisp/setup.lisp")
         (load "~/quicklisp/setup"))
        (t (format t "Installing Quicklisp...")
           (load "http://beta.quicklisp.org/quicklisp.lisp")
           (funcall (find-symbol "INSTALL" :quicklisp-quickstart)))))

(ql:quickload "lisplog")
(ql:quickload "swank")

(defun ll ()
  (swank:set-package :lisplog))

(ll)

(start-rss-reader-thread)

(defun start-swank (&optional port)
  (when port
    (swank:create-server :port port :dont-close t)))

(defun reload ()
  (ql:quickload "lisplog" :verbose t))
