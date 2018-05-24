;;;; cdb.lisp

(in-package #:quicklisp-client-tests)

(defvar *services-cdb*
  (merge-pathnames "cdb/services.cdb"
                   *base-directory*))

(defvar *simple-cdb*
  (merge-pathnames "cdb/simple.cdb"
                   *base-directory*))

(deftest test-cdb-lookup ()
  (is (equalp "22" (ql-cdb:lookup "ssh/tcp" *services-cdb*)))
  (is (equalp "23" (ql-cdb:lookup "telnet/tcp" *services-cdb*))))

(deftest test-map-cdb ()
  (ql-cdb::map-cdb (lambda (key value)
                     (is (equalp key value)))
                   *simple-cdb*))

(deftest test-write-cdb ()
  (let ((cdb
          (ql-cdb:convert-index-file (merge-pathnames "cdb/test-index.txt"
                                                      *base-directory*))))
    (with-open-file (stream cdb :element-type '(unsigned-byte 8))
      (ql-cdb::map-cdb
       (lambda (key value)
         ;; test input file maps "foo" to "foo foo" for each line
         (is (equalp (format nil "~A ~A" key key) value)))
       stream))
    (delete-file cdb)))

