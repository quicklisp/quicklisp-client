;;;; key-management.lisp

(in-package #:ql-openpgp)

(define-condition unknown-openpgp-key-id (error)
  ((key-id
    :initarg :key-id
    :reader unknown-openpgp-key-id-key-id))
  (:report (lambda (condition stream)
             (format stream "Unknown OpenPGP key id ~S"
                     (unknown-openpgp-key-id-key-id condition)))))

(defun all-key-files ()
  (let* ((key-directory (ql-setup:qmerge "openpgp-keys/"))
         (wild (make-pathname :name :wild
                              :type "asc"
                              :version :wild
                              :defaults key-directory)))
    (directory wild)))

(defun all-keys ()
  (flet ((try-file (file)
           (ignore-errors (load-packet-from-file file))))
    (loop for file in (all-key-files)
          for packet = (try-file file)
          when packet collect packet)))

(defun find-key (key-id)
  "Returns the OpenPGP key associated with KEY-ID in the system. If no
key by that id is found, signals a continuable error of type
UNKNOWN-OPENPGP-KEY-ID."
  (loop
    (flet ((match (packet)
             (and (typep packet 'rsa-public-key-packet)
                  (equal (key-id-string packet) key-id))))
      (let ((key (find-if #'match (all-keys))))
        (if key
            (return key)
            (cerror "Try again"
                    'unknown-openpgp-key-id
                    :key-id key-id))))))
