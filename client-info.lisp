;;;; client-info.lisp

(in-package #:quicklisp-client)

(defparameter *client-base-url* "http://zeta.quicklisp.org/")

(defclass client-info ()
  ((setup-url
    :reader setup-url
    :initarg :setup-url)
   (asdf-url
    :reader asdf-url
    :initarg :asdf-url)
   (canonical-client-info-url
    :reader canonical-client-info-url
    :initarg :canonical-client-info-url)
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
    :initarg :plist)
   (source-file
    :reader source-file
    :initarg :source-file)))

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
                              canonical-client-info-url
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
                     :canonical-client-info-url canonical-client-info-url
                     :client-tar-url client-tar-url
                     :version version
                     :subscription-url subscription-url
                     :plist plist
                     :source-file (probe-file file)))))

(defun fetch-client-info (url)
  (let ((info-file (qmerge "tmp/client-info.sexp")))
    (delete-file-if-exists info-file)
    (fetch url info-file :quietly t)
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

(defun client-version-lessp (client-info-1 client-info-2)
  (string-lessp (version client-info-1)
                (version client-info-2)))

(defun client-version ()
  "Return the version for the current local client installation. May
or may not be suitable for passing as the :VERSION argument to
INSTALL-CLIENT, depending on if it's a standard Quicklisp-provided
client."
  (version (local-client-info)))

(defun client-url ()
  "Return an URL suitable for passing as the :URL argument to
INSTALL-CLIENT for the current local client installation."
  (canonical-client-info-url (local-client-info)))
