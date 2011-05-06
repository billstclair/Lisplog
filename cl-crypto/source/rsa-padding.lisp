(in-package :cl-crypto)

;;;
;;; Padding - just PKCS1 Type 2 at this time
;;;

(defun rsa-padding-add-pkcs1-type-2 (in out)
  (with-random-byte-stream
    (let* ((in-bytes (array-total-size in))
           (out-bytes (array-total-size out))
           (pad-bytes (- out-bytes in-bytes))
           (pad-bytes-1 (1- pad-bytes)))
      (when (< pad-bytes 11)
        (error "RSA data too large for key size"))
      (setf (aref out 0) 0
            (aref out 1) 2
            (aref out pad-bytes-1) 0)
      (do ((i 2 (1+ i)))
          ((= i pad-bytes-1))
        (setf (aref out i)
              (do ((r (get-random-bits 8) (get-random-bits 8)))
                  ((not (zerop r)) r))))
      (dotimes (i in-bytes)
        (setf (aref out (+ i pad-bytes)) (aref in i)))
      out)))

(defun rsa-padding-strip-pkcs1-type-2 (in out)
  (let ((in-bytes (array-total-size in)))
    (when (< (array-total-size out) (- in-bytes 11))
      (error "Output array too small for unpadded data"))
    (when (not (and (= 0 (aref in 0))
		    (= 2 (aref in 1))))
      (error "Invalid PKCS padding signature"))
    (let ((offset 0))
      (do ((i 2 (1+ i)))
	  ((= i in-bytes))
	(when (= 0 (aref in i))
	  (setq offset (1+ i))
	  (return-from nil)))
      (when (or (= offset (1- in-bytes))
		(> offset (1- in-bytes)))
	(error "Missing null prior to data"))
      (when (< offset 8)
	(error "Bad pad byte count"))
      (dotimes (i (- in-bytes offset))
	(setf (aref out i) (aref in (+ offset i)))))
    out))
