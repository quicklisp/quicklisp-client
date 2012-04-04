;;;; packed.lisp

;;;
;;; Support for finding systems inside tarballs
;;;

(in-package #:quicklisp-client)

(defun packed-projects-searcher (system-name)
  "This function is added to ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*
to use the packed projects anywhere in ASDF:*CENTRAL-REGISTRY*."
  (let ((tar (qmerge "tmp/release-install.tar")))
    (dolist (dir asdf:*central-registry*)
      (dolist (archive (merge-pathnames "*.tar.gz" dir))
        (unless (probe-directory (subseq (namestring archive) 0
                                         (- (length (namestring archive)) 7)))
          (ensure-directories-exist tar)
          (gunzip archive tar)
          (pushnew (unpack-tarball tar :directory dir)
                   asdf:*central-registry*
                   :test #'string=
                   :key (lambda (path)
                          (etypecase path
                            (stringp path)
                            (pathname (namestring path)))))
          (asdf:find-system system-name))))))
