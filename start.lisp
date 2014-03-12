(in-package :cl-user)

(unless (find-package "QUICKLISP")
  (cond ((probe-file "~/quicklisp/setup.lisp")
         (load "~/quicklisp/setup"))
        (t (format t "Installing Quicklisp...")
           (load "http://beta.quicklisp.org/quicklisp.lisp")
           (funcall (find-symbol "INSTALL" :quicklisp-quickstart)))))

(load "lisplog.asd")
(ql:quickload "lisplog")

(defun ll ()
  (in-package :lisplog))

(ll)

(start-rss-reader-thread)

(defun start-swank (&optional port)
  (when port
    (ql:quickload "swank")
    (funcall (find-symbol "CREATE-SERVER" :swank)
             :port port :dont-close t)))

(defun reload ()
  (ql:quickload "lisplog" :verbose t))
