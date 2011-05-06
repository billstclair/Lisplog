(in-package :cl-crypto)

;;;
;;; Prime number generation
;;;

;; (defun gen-prime (num-bits &key secret)
;;   (princ "--P--")
;;   (with-random-byte-stream
;;     (flet ((foo ()
;;              (let ((result (logior #b1 (get-random-bits num-bits))))
;;                (when secret 
;;                  (setq result (logior result (ash #b11 (- num-bits 2)))))
;;                result)))
;;       (declare (dynamic-extent #'foo))
;;       (let ((b (byte 1 num-bits)))
;;         (loop
;;            (let ((p (foo)))
;;              (loop
;;                 (when (verify-prime p)
;;                   (return-from gen-prime p))
;;                 (incf p 2)
;;                 (when (ldb b p) (return)))))))))

(defun gen-prime (num-bits &key secret)
  (princ "--P--")
  (with-random-byte-stream
    (flet ((foo ()
             (let ((result (logior #b1 (get-random-bits num-bits))))
               (when secret 
                 (setq result (logior result (ash #b11 (- num-bits 2)))))
               result)))
      (declare (dynamic-extent #'foo))
      (loop
	 (let ((p (foo)))
	   (let ((step (small-prime-test p)))
	     (when step (setq p (+ p step))
		   (when (verify-prime p)
		     (return-from gen-prime p)))))))))
	     

(defmacro gen-secret-prime (num-bits)
  `(gen-prime ,num-bits :secret t))

(defun fermat-little-test (p)
  (let ((p-minus-1 (1- p)))
    (if (= 1 (modulo-pow 2 p-minus-1 p))
	(progn
	  (dotimes (i 5)
	    (let ((a (get-ranged-random-num 1 p-minus-1)))
	      (when (not (= 1 (modulo-pow a p-minus-1 p)))
		(return-from fermat-little-test nil))))
	  t)
	nil)))

(defun verify-prime (p)
  (and (oddp p)
;;       (small-prime-test-naive p)
       (fermat-little-test p)
       (rabin-miller p 5)))

;;
;; Uses trial division to check if any number in range 2 < x < (sqrt p)
;; divides p
;;
;; Useful only as a sanity check against very small primes.
;;
(defun prime-check-naive (p)
  (let ((stop (ceiling (sqrt p))))
    (do ((i 3 (1+ i)))
	((> i stop) t)
      (when (zerop (mod p i))
	(return-from prime-check-naive nil)))))

(defun witness (a n)
  (princ "W")
  (let ((n-minus-1 (1- n))
	(d 1)
	(x 0))
    (do ((i (1- (integer-length n-minus-1)) (1- i)))
	((< i 0))
      (setq x d
	    d (mod (square d) n))
      (when (and (= d 1) (not (= x 1)) (not (= x n-minus-1)))
	(princ "--W-FAIL--")
	(return-from witness t))
      (when (logbitp i n-minus-1)
	(setq d (mod (* d a) n))))
    (if (not (= d 1))
	(progn
	  (princ "--WF--")
	  t)
	nil)))
	 
  
(defun rabin-miller (n k)
  (princ "R")
  (let* ((n-minus-1 (1- n)))
    (dotimes (i k)
      (when (witness (get-ranged-random-num 1 n-minus-1)  n)
	(return-from rabin-miller nil)))
    t))
			     


