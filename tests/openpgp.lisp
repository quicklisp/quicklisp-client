;;;; openpgp.lisp

(in-package #:quicklisp-client-tests)

(defun load-signature (file)
  (let ((signature-file (format nil "~A.asc" (namestring file))))
    (ql-openpgp::load-signature signature-file)))

(defun load-key-by-id (id)
  (let* ((key-directory (merge-pathnames "openpgp/keys/"
                                         *base-directory*))
         (key-file (make-pathname :name id
                                  :type "key"
                                  :defaults key-directory)))
    (if (probe-file key-file)
        (ql-openpgp::load-public-key key-file)
        (error "No key file found for id ~S" id))))

(defun check-signature (file)
  (let* ((sig (load-signature file))
         (key (load-key-by-id (ql-openpgp::key-id-string sig))))
    (ql-openpgp:verify-signature file sig key)))

(defun signature-check-files ()
  (let ((directory (merge-pathnames "openpgp/signed/"
                                    *base-directory*)))
    (directory (merge-pathnames "*.sha512" directory))))

(deftest test-openpgp-valid-signatures ()
  (dolist (file (signature-check-files))
    (is (eql :good-signature (check-signature file)))))

(deftest test-load-quicklisp-releases-key ()
  (let* ((file (merge-pathnames "openpgp/keys/quicklisp-releases.asc"))
         (key (ql-openpgp::load-public-key key)))
    (is (equalp (ql-openpgp::packet-type key) :public-key))
    (is (equalp (ql-openpgp::key-id-string key) "307965ab028b5ff7"))))
