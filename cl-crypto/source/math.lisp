(in-package :cl-crypto)

;;;
;;; Math routines used by RSA and primality tests
;;; 

(defun extended-gcd (a b)
  (if (zerop (mod a b))
      (values 0 1)
      (multiple-value-bind (x y) (extended-gcd b (mod a b))
	(values y (- x (* y (floor a b)))))))

(defun modulo-inverse (a n)
  (multiple-value-bind (x y) (extended-gcd a n)
    (declare (ignore y))
    (mod x n)))

(defun totient (p q)
  (* (1- p) (1- q)))

(defun square (x)
  (* x x))

(defun modulo-pow (base exp n)
  (if (zerop exp)
      1
      (if (oddp exp)
	  (mod (* base (modulo-pow base (- exp 1) n)) n)
	  (mod (square (modulo-pow base (/ exp 2) n)) n))))

(defun modulo-exp (base exp n)
  (let ((c 0) (d 1) (k (1- (integer-length exp))))
    (do ((i k (1- i)))
	((< i 0))
      (setq c (* c 2)
	    d (mod (square d) n))
      (when (logbitp i exp)
	(setq c (1+ c)
	      d (mod (* d base) n))))
    d))


	

