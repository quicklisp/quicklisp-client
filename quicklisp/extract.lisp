(in-package #:ql-extract)

(defvar *extract-type-functions*
  '(("tar" . tar-extract)
    ("gz" . tgz-extract))
  "assoc list to decide which extract-function are called by EXTRACT function.")

(defun tar-extract (archive directory &key &allow-other-keys)
  "extract tar."
  (unpack-tarball archive :directory directory))

(defun tgz-extract (archive directory &key temp-tar)
  "extract tgz."
  (ensure-directories-exist temp-tar)
  (gunzip archive temp-tar)
  (tar-extract temp-tar directory)
  (delete-file temp-tar))

(defun extract (archive directory &rest rest)
  "extract archive depends on type of a file."
  (let* ((name (namestring archive))
         (type (subseq name
                       (1+ (position
                            #\. name :from-end t))))
         (call (cdr (assoc type *extract-type-functions* :test 'equal))))
    (if call
        (apply call archive directory rest)
        (error "Unknown archive type ~S" archive))))
