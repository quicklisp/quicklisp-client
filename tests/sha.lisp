;;;; sha.lisp

(in-package #:quicklisp-client-tests)

(defparameter *base-directory*
  (asdf:system-source-directory "quicklisp-client-tests"))

;;; Hash vector directories consist of an index file (named
;;; index.sha1, index.sha256, or index.sha512) and a number of data
;;; files named 0000.sha1 (or .sha256, or .sha512), 0001.sha1,
;;; etc. The first data file is empty and is used to test the empty
;;; hash digest result. Other files contain random amounts of random
;;; data. For each data file, there is a line in the index file
;;; containing its sha digest as computed by the ironclad library.
;;;
;;; These tests are to confirm that Quicklisp's hashing matches
;;; ironclad's.

(defun vector-hashes (vector-directory sha-class)
  "Load the hash index file in VECTOR-DIRECTORY and return its hashes
as a list."
  (let ((file (make-pathname :name "index"
                             :type (string-downcase sha-class)
                             :defaults vector-directory)))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line
            collect line))))

(defun vector-info (vector-directory sha-class)
  "Return a list of (pathname . hash-string) values; the file with the
given pathname should hash to the given hash string."
  (let ((index -1))
    (mapcar (lambda (hash)
              (cons (make-pathname :name (format nil "~4,'0D" (incf index))
                                   :type (string-downcase sha-class)
                                   :defaults vector-directory)
                    hash))
            (vector-hashes vector-directory sha-class))))

(defun file-sha-equal (sha1 sha2)
  (equalp (cdr sha1) sha2))

(deftest verify-vector-info ()
  (dolist (sha-class '(sha1 sha256 sha512))
    (let* ((subdir (list :relative (format nil "~(~A~)-vectors" sha-class)))
           (data (vector-info (merge-pathnames (make-pathname
                                                :directory subdir)
                                               *base-directory*)
                              sha-class)))
      (loop for (file . expected-digest) in data
            do
               (is (file-sha-equal
                    (cons file (file-sha-string sha-class file))
                    expected-digest))))))

