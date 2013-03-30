;;;; -*- mode: lisp; package: iso8601-date -*-

;; Author: Thomas Russ
;; Date: October 29, 2004
;; Copyright: This code is placed in the public domain
;;
;; Modified: September 8, 2006 by Andrew Philpot
;; Recognize and discard any fractional seconds

(in-package :lisplog)

(defun format-iso8601-time (time-value &key (timezone 0) (include-timezone-p t))
  "Formats a universal time TIME-VALUE in ISO 8601 format, with the time zone
included if INCLUDE-TIMEZONE-P is non-NIL"
  (flet ((format-iso8601-timezone (zone dst)
           (when dst (decf zone))
           (if (zerop zone)
               "Z"
               (multiple-value-bind (h m) (truncate (abs zone) 1.0)
                 ;; Tricky. Sign of time zone is reversed in ISO 8601
                 ;; relative to Common Lisp convention!
                 (format nil "~:[+~;-~]~2,'0D:~2,'0D"
                         (> zone 0) h (round m))))))
    (multiple-value-bind (second minute hour day month year dow dst zone)
        (decode-universal-time time-value timezone)
      (declare (ignore dow))
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D~:[~*~;~A~]"
              year month day hour minute second
              include-timezone-p (format-iso8601-timezone zone dst)))))

(defun parse-iso8601-time (time-string)
  "Parses an ISO 8601 format string and returns the universal time"
  (flet ((parse-delimited-string (string delimiters n)
           ;; Parses a delimited string and returns a list of n integers found in that string.
           (let ((answer (make-list n :initial-element 0)))
             (when (> (length string) 0)
               (loop for i upfrom 0 below n
                  for start = 0 then (1+ end)
                  for end = (position-if #'(lambda (delim)
                                             (member delim delimiters))
                                         string :start (1+ start))
                  do (setf (nth i answer)
                           (parse-integer (subseq string start end)
                                          :junk-allowed t))
                  when (null end) return t))
             (values-list answer)))
         (parse-fixed-field-string (string field-sizes)
           ;; Parses a string with fixed length fields and returns a list of integers found in that string.
           (let ((answer (make-list (length field-sizes) :initial-element 0)))
             (loop with len = (length string)
                for start = 0 then (+ start field-size)
                for field-size in field-sizes
                for i upfrom 0
                while (< start len)
                do (setf (nth i answer)
                         (parse-integer (subseq string start (+ start field-size)))))
             (values-list answer))))
    (flet ((parse-iso8601-date (date-string)
             (let ((hyphen-pos (position #\- date-string)))
               (if hyphen-pos
                   (parse-delimited-string date-string '(#\-) 3)
                   (parse-fixed-field-string date-string '(4 2 2)))))
           (parse-iso8601-timeonly (time-string)
             (let* ((colon-pos (position #\: time-string))
                    (zone-pos (or (position #\- time-string)
                                  (position #\+ time-string)
                                  (position #\Z time-string)))
                    (timeonly-string (subseq time-string 0 zone-pos))
                    (zone-string (when zone-pos (subseq time-string (1+ zone-pos))))
                    (time-zone nil))
               (when zone-pos
                 (multiple-value-bind (zone-h zone-m)
                     (parse-delimited-string zone-string '(#\: #\.) 2)
                   (setq time-zone (+ zone-h (/ zone-m 60)))
                   (when (char= (char time-string zone-pos) #\-)
                     (setq time-zone (- time-zone)))))
               (multiple-value-bind (hh mm ss)
                   (if colon-pos
                       (parse-delimited-string timeonly-string '(#\: #\.) 3)
                       (parse-fixed-field-string timeonly-string '(2 2 2)))
                 (values hh mm ss time-zone)))))
      (let ((time-separator (position #\T time-string)))
        (multiple-value-bind (year month date)
            (parse-iso8601-date
             (subseq time-string 0 time-separator))
          (if time-separator
              (multiple-value-bind (hh mm ss zone)
                  (parse-iso8601-timeonly
                   (subseq time-string (1+ time-separator)))
                (if zone
                    ;; Tricky: Sign of time zone is reversed in ISO 8601
                    ;; relative to Common Lisp convention!
                    (encode-universal-time ss mm hh date month year (- zone))
                    (encode-universal-time ss mm hh date month year)))
              (encode-universal-time 0 0 0 date month year)))))))

;;;; iso8601.lisp ends here
