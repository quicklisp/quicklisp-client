;;;; generate-vectors.lisp

(defpackage #:generate-test-vectors
  (:use #:cl #:ironclad))

(in-package #:generate-test-vectors)

(defun write-one-vector (output-file digest count)
  "Write COUNT random octets to OUTPUT-FILE and return the digest as a
  string."
  (let ((d (make-digest digest))
        (v (make-array 1 :element-type '(unsigned-byte 8))))
    (with-open-file (stream output-file :direction :output
                                        :if-exists :error
                                        :element-type '(unsigned-byte 8))
      (dotimes (i count)
        (let ((byte (random 256)))
          (write-byte byte stream)
          (setf (aref v 0) byte)
          (update-digest d v)))
      (string-downcase
       (byte-array-to-hex-string (produce-digest d))))))

(defun generate-vectors (digest directory
                         &key min-size max-size count type)
  (ensure-directories-exist directory)
  (let ((index (make-pathname :name "index"
                              :type type
                              :defaults directory)))
    (with-open-file (stream index :direction :output
                                  :if-exists :error)
      (dotimes (i count)
        (let ((vector-file (make-pathname :name (format nil "~4,'0D" i)
                                          :type type
                                          :defaults directory))
              ;; This is to exercise the "zero bytes digested" edge
              ;; case
              (count (if (zerop i)
                         0
                         (+ min-size (random (- max-size min-size))))))
          (let ((digest (write-one-vector vector-file digest count)))
            (write-line digest stream)))))
    (probe-file index)))

