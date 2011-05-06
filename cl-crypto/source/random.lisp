(in-package :cl-crypto)

;;;
;;; Random number generation
;;;


(defparameter +linux-urandom-dev+ #P"/dev/urandom")
(defvar *random-byte-stream* nil)

(defmacro with-random-byte-stream (&body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-random-byte-stream #',thunk))))

(defun call-with-random-byte-stream (thunk)
  (if *random-byte-stream*
      (funcall thunk)
      (with-open-file (*random-byte-stream*
                       +linux-urandom-dev+
                       :element-type '(unsigned-byte 8))
        (funcall thunk))))

(defun get-random-bits (num-bits)
  (with-random-byte-stream
    (let ((num-bytes (ceiling num-bits 8))
          (result 0)
          (in *random-byte-stream*))
      (dotimes (i num-bytes)
        (setq result (logior result (ash (read-byte in) (* i 8)))))
      result)))

(defun random-integer (ceiling)
  ;(princ ".")
  (let* ((nbits (* 8 (ceiling (integer-length (1+ ceiling)) 8)))
         (x (get-random-bits nbits))
         (n (ash 1 nbits)))
    (floor (* x (1+ ceiling)) n)))

;;
;; Returns a random number x such that:
;;	floor <= x <= ceiling
;;
(defun get-ranged-random-num (floor ceiling)
  (+ floor (random-integer (- ceiling floor))))
