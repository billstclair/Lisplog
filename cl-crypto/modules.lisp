;;;; -*- mode: lisp -*-

(in-package :module-manager-user)

;;;
;;; cl-crypto module definition for GBBopen
;;;

;; Required packages for client
(require :asdf)
(asdf:oos 'asdf:load-op 'cl-base64)
(asdf:oos 'asdf:load-op 'flexi-streams)

(with-system-name (:cl-crypto)

  (define-root-directory :cl-crypto  *load-truename*)
  
  (define-module :cl-crypto
    (:directory :cl-crypto)
    (:files "packages"
            "types"
            "utility"
	    "math"
	    "random"
	    "small-primes"
	    "prime"
	    "rsa"
	    "rsa-padding"
            "aes16"
            "sha1"
            "strings"
	    )))


