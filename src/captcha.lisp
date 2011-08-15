; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Captcha logic
;;;

(in-package :lisplog)

(defstruct (captcha (:constructor %make-captcha))
  query-explanation
  query-html
  response-size
  hidden-value)

(defun captcha-values (captcha)
  (values (captcha-query-explanation captcha)
          (captcha-query-html captcha)
          (captcha-response-size captcha)
          (captcha-hidden-value captcha)))

(defun read-captcha-state (&optional (db *data-db*))
  (sexp-get db $CAPTCHA $CAPTCHA :subdirs-p nil))

(defun (setf read-captcha-state) (state &optional (db *data-db*))
  (setf (sexp-get db $CAPTCHA $CAPTCHA :subdirs-p nil) state))

;; New random seed every 10 minutes
(defparameter *captcha-valid-time* (* 60 10))

(defun make-captcha-seed ()
  (format nil "~x" (cl-crypto:get-random-bits 160)))

(defun parse-hex (string)
  (parse-integer string :radix 16))

(defun update-captcha-state (&optional (db *data-db*))
  (let* ((state (read-captcha-state db))
         (time (getf state :time))
         (seed (getf state :seed))
         (last-time (getf state :last-time))
         (last-seed (getf state :last-seed))
         (now (get-unix-time)))
    (when (or (null time) (< time (- now *captcha-valid-time*)))
      (let ((new-seed (make-captcha-seed)))
        (setf last-time (or time now)
              last-seed (or seed new-seed)
              time now
              seed new-seed
              (getf state :last-time) last-time
              (getf state :last-seed) last-seed
              (getf state :time) time
              (getf state :seed) seed
              (read-captcha-state db) state)))
    (values time seed last-time last-seed)))

(defun get-captcha-seed (timestamp &optional (db *data-db*))
  (unless (< timestamp (- (get-unix-time) *captcha-valid-time*))
    (multiple-value-bind (time seed last-time last-seed)
        (update-captcha-state db)
      (declare (ignore last-time))
      (cond ((< timestamp time) last-seed)
            (t seed)))))

(defun make-captcha (&optional (db *data-db*))
  (cl-crypto:with-random-byte-stream
    (multiple-value-bind (time seed) (update-captcha-state db)
      (let* ((x (1+ (cl-crypto:random-integer 9)))
             (y (1+ (cl-crypto:random-integer 9)))
             (opnum (cl-crypto:random-integer 3))
             (op (cond ((eql opnum 0) '+)
                       ((eql opnum 1)
                        (when (< x  y) (rotatef x y))
                        '-)
                       (t '*)))
             (opname (if (eq op '*) "x" op))
             (res (funcall op x y))
             (query (format nil "~d ~a ~d = " x opname y))
             (seed-int (parse-hex seed))
             (res-hash (cl-crypto:sha1 (format nil "~d" res)))
             (res-int (parse-hex res-hash))
             (hidden-int (logxor seed-int res-int))
             (hidden-hash (cl-crypto:sha1 (format nil "~x" hidden-int))))
        (%make-captcha
         :query-explanation "Solve the simple arithmetic problem."
         :query-html query
         :response-size 4
         :hidden-value (format nil "~a+~a" time hidden-hash))))))

(defun validate-captcha (res hidden-value &optional (db *data-db*))
  (check-type res string)
  (check-type hidden-value string)
  (let* ((pos (position #\+ hidden-value))
         (time-str (and pos (subseq hidden-value 0 pos)))
         (timestamp (ignore-errors (parse-integer time-str)))
         (seed (and timestamp (get-captcha-seed timestamp db)))
         (seed-int (and seed (parse-hex seed)))
         (hidden-hash (and pos (subseq hidden-value (1+ pos))))
         (res-hash (cl-crypto:sha1 res))
         (res-int (parse-hex res-hash)))
    (cond ((and seed-int hidden-hash)
           (let ((hidden-int (logxor seed-int res-int)))
             (equal hidden-hash
                    (cl-crypto:sha1 (format nil "~x" hidden-int)))))
          (t (values nil :timeout)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2011 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
