;;;; client-update.lisp

(in-package #:quicklisp-client)

(defparameter *client-base-url* "http://zeta.quicklisp.org/")

(defclass client-info ()
  ((setup-url
    :reader setup-url
    :initarg :setup-url)
   (asdf-url
    :reader asdf-url
    :initarg :asdf-url)
   (client-tar-url
    :reader client-tar-url
    :initarg :client-tar-url)
   (version
    :reader version
    :initarg :version)
   (subscription-url
    :reader subscription-url
    :initarg :subscription-url)
   (plist
    :reader plist
    :initarg :plist)))

(defmethod print-object ((client-info client-info) stream)
  (print-unreadable-object (client-info stream :type t)
    (prin1 (version client-info) stream)))

(defun format-client-url (path &rest format-arguments)
  (if format-arguments
      (format nil "~A~{~}" *client-base-url* path format-arguments)
      (format nil "~A~A" *client-base-url* path)))

(defun client-info-url-from-version (version)
  (format-client-url "client/~A/client-info.sexp" version))

(define-condition invalid-client-info (error)
  ((plist
    :initarg plist
    :reader invalid-client-info-plist)))

(defun load-client-info (file)
  (let ((plist (safely-read-file file)))
    ;; FIXME: Should institute some kind of client-info plist format
    ;; versioning & checking
    (destructuring-bind (&key setup-url asdf-url
                              client-tar-url subscription-url
                              version
                              &allow-other-keys)
        plist
      (unless (and setup-url asdf-url client-tar-url version)
        (error 'invalid-client-info
               :plist plist))
      (make-instance 'client-info
                     :setup-url setup-url
                     :asdf-url asdf-url
                     :client-tar-url client-tar-url
                     :version version
                     :subscription-url subscription-url
                     :plist plist))))

(defun fetch-client-info (url)
  (let ((info-file (qmerge "tmp/client-info.sexp")))
    (delete-file-if-exists info-file)
    (fetch url info-file)
    (handler-case
        (load-client-info info-file)
      ;; FIXME: So many other things could go wrong here; I think it
      ;; would be nice to catch and report them clearly as bogus URLs
      (invalid-client-info ()
        (error "Invalid client info URL -- ~A" url)))))

(defun local-client-info ()
  (load-client-info (qmerge "client-info.sexp")))

(defun newest-client-info (&optional (info (local-client-info)))
  (let ((latest (subscription-url info)))
    (when latest
      (fetch-client-info latest))))

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


(defun client-version-lessp (client-info-1 client-info-2)
  (string-lessp (version client-info-1)
                (version client-info-2)))

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
    (replace-file local-asdf (qmerge "asdf.lisp"))))

(defun update-client (&key (prompt t))
  (let* ((local-info (local-client-info))
         (newest-info (newest-client-info local-info)))
    (cond ((null newest-info)
           (format t "No updates for this client are available.~%"))
          ((client-version-lessp local-info newest-info)
           (format t "Updating client from version ~A to version ~A.~%"
                   (version local-info)
                   (version newest-info))
           (when (or (not prompt)
                     (press-enter-to-continue))
             (install-client newest-info local-info)))
          (t
           (format t "The most up-to-date client is already installed.~%")
           )))
  t)
