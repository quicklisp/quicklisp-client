;;;; key-management.lisp

(in-package #:ql-openpgp)

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
  (flet ((match (packet)
           (and (typep packet 'rsa-public-key-packet)
                (equal (key-id-string packet) key-id))))
    (find-if #'match (all-keys))))



