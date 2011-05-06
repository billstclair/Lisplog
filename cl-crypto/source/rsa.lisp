;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RSA implementation in Common Lisp
;;;
;;; Author: mrbug@rayservers.net
;;;
;;; Date: 22 NOV 2010
;;;
;;; References: many
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-crypto)

(defparameter +rsa-public-exponent+ 65537)


;;;
;;; Data conversion utilities
;;;

(defun num->byte-array (num)
  (let* ((num-bytes (truncate (+ (integer-length num) 7) 8))
	 (num-bits (* num-bytes 8))
	 (out (make-array num-bytes :element-type '(unsigned-byte 8))))
    (dotimes (i num-bytes)
      (setf (aref out i) (ldb (byte 8 (- num-bits (* (1+ i) 8))) num)))
    out))


;;;
;;; RSA implementation
;;;

;;
;; Simple encryption
;;
;;	c = m^e mod n
;;
(defun rsa-encrypt (m e n)
  (modulo-pow m e n))

;;
;; Simple decryption
;;
;;	m = c^d mod n
;;
(defun rsa-decrypt-naive (c d n)
  (modulo-pow c d n))

;;
;; Faster decryption using Chinese remainder theorem
;; 
;;	m = c^d mod n
;;
;; Or, using CRT:
;;
;;	m1 = c ^ (d mod (p-1)) mod p
;;	m2 = c ^ (d mod (q-1)) mod q
;;	h = u * u * (m2-m1) mod p
;;	m = m1 + h * p
;;
;; Where:
;;
;;	u = q^-1 mod p
;;
;;	otherwise known as the modular multiplicative inverse 
;;	of q mod p.  It is pre-computed at key generation time and
;;	stored with the private key.

(defun rsa-decrypt (c d u p q)
  (let* ((m1 (modulo-pow c (mod d (1- p)) p))
	 (m2 (modulo-pow c (mod d (1- q)) q))
	 (h (mod (* u (- m2 m1)) p)))
    (+ m1 (* h p))))


;;
;; RSA keypair generation
;;
;; Returns e p q n d u
;;
;; Where:
;;	e is the public exponent
;;	p, q are the random prime factors of the modulus n
;;	n is the modulus of bitlength numbits
;;	d is the private exponent
;;	u is a pre-computed value used in the Chinese remainder theorum
;;	    (CRT) method of decryption
;;
;;	(e n) is the public keypair
;;	(d n) is the private keypair
;;	(d n p q u) should be retained to facilitate CRT fast decryption
;;

(defun rsa-gen (numbits &optional verbose)
  (with-random-byte-stream
    (let ((halfbits (/ numbits 2))
          (e +rsa-public-exponent+))
      (flet ((foo ()
               (do ((tmp (gen-secret-prime halfbits) (gen-secret-prime halfbits)))
                   ((= 1 (gcd e (1- tmp))) tmp))))
        (declare (dynamic-extent #'foo))
        (let* ((p (foo))
               (q (foo))
               (n (* p q))
               (d (modulo-inverse e (totient p q)))
               (u (modulo-inverse q p)))
          (when verbose
            (format t "~%p=~X~%q=~X" p q)
            (finish-output))
          (when (not (= (integer-length n) numbits)) (error "len(n) != numbits"))
          (when (= p q) (error "p == q"))
          (when (> q p)
            (let ((tmp q))
              (setq q p
                    p tmp)))
          (values e p q n d u))))))	 


;;;
;;; RSA tests
;;;
      
(defun %rsa-self-test-step (m e p q n d u)
  (let* ((c (rsa-encrypt m e n))
	 (pm (rsa-decrypt c d u p q)))
    (= m pm)))

(defun rsa-self-test (num-bits &key (iterations 10) (encrypts 10))
  (dotimes (i iterations t)
    (multiple-value-bind (e p q n d u) (rsa-gen num-bits t)
      (dotimes (j encrypts)
        (unless (%rsa-self-test-step
                 (get-random-bits (ash num-bits -2))
                 e p q n d u)
          (return-from rsa-self-test nil))))))

  
