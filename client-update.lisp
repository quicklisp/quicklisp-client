;;;; client-update.lisp

(in-package #:quicklisp-client)

(defun retirement-directory (base)
  (let ((suffix 0))
    (loop
      (incf suffix)
      (let* ((try (format nil "~A-~D" base suffix))
             (dir (qmerge (make-pathname :directory
                                         (list :relative "retired" try)))))
        (unless (probe-directory dir)
          (return dir))))))

(defun retire (directory base)
  (let ((retirement-home (qmerge "retired/"))
        (from (truename directory)))
    (ensure-directories-exist retirement-home)
    (let* ((*default-pathname-defaults* retirement-home)
           (to (retirement-directory base)))
      (rename-directory from to)
      to)))

(defun client-update-scratch-directory (client-info)
  (qmerge (make-pathname :directory
                         (list :relative
                               "tmp"
                               "client-update"
                               (version client-info)))))

(defun install-client (newest-info local-info)
  (let* ((work-directory (client-update-scratch-directory newest-info))
         (current-quicklisp-directory (qmerge "quicklisp/"))
         (new-quicklisp-directory
          (merge-pathnames "quicklisp/" work-directory))
         (local-temp-tar (merge-pathnames "quicklisp.tar" work-directory))
         (local-setup (merge-pathnames "setup.lisp" work-directory))
         (local-asdf (merge-pathnames "asdf.lisp" work-directory)))
    (ensure-directories-exist work-directory)
    (fetch (client-tar-url newest-info) local-temp-tar)
    (unpack-tarball local-temp-tar :directory work-directory)
    ;; FIXME: could compare URLs or some other property to avoid
    ;; downloading these every time.
    (fetch (setup-url newest-info) local-setup)
    (fetch (asdf-url newest-info) local-asdf)
    (retire (qmerge "quicklisp/")
            (format nil "quicklisp-~A"
                    (version local-info)))
    (rename-directory new-quicklisp-directory current-quicklisp-directory)
    (replace-file local-setup (qmerge "setup.lisp"))
    (replace-file local-asdf (qmerge "asdf.lisp"))
    (replace-file (source-file newest-info) (qmerge "client-info.sexp"))))

(defun update-client (&key (prompt t))
  (let* ((local-info (local-client-info))
         (newest-info (newest-client-info local-info)))
    (cond ((null newest-info)
           (format t "No client update available.~%"))
          ((client-version-lessp local-info newest-info)
           (format t "Updating client from version ~A to version ~A.~%"
                   (version local-info)
                   (version newest-info))
           (when (or (not prompt)
                     (press-enter-to-continue))
             (install-client newest-info local-info)))
          (t
           (format t "The most up-to-date client, version ~A, ~
                      is already installed.~%"
                   (version local-info)))))
  t)
