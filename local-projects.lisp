;;;; local-projects.lisp

;;;
;;; Local project support.
;;;
;;; Local projects can be placed in <quicklisp>/local-projects/. New
;;; entries in that directory are automatically scanned for system
;;; files for use with QL:QUICKLOAD.
;;;
;;; This works by keeping a cache of system file pathnames in
;;; <quicklisp>/local-projects/system-index.txt. Whenever the
;;; timestamp on the local projects directory is newer than the
;;; timestamp on the system index file, the entire tree is re-scanned
;;; and cached.
;;;
;;; This will pick up system files that are created as a result of
;;; creating new project directory in <quicklisp>/local-projects/,
;;; e.g. unpacking a tarball or zip file, checking out a project from
;;; version control, etc. It will NOT pick up a system file that is
;;; added sometime later in a subdirectory; for that, the
;;; REGISTER-LOCAL-PROJECTS function is needed to rebuild the system
;;; file index.
;;;
;;; In the event there are multiple systems of the same name in the
;;; directory tree, the one with the shortest pathname namestring is
;;; used. This is intended to ignore stuff like _darcs pristine
;;; directories.
;;;
;;; Work in progress!
;;;

(in-package #:quicklisp-client)

(defparameter *local-projects-directory*
  (qmerge "local-projects/"))

(defun system-index-file (pathname)
  (merge-pathnames "system-index.txt" pathname))

(defun local-project-system-files (pathname)
  (let ((wild (merge-pathnames "**/*.asd" pathname)))
    (sort (directory wild)
          #'<
          :key (lambda (file)
                 (length (namestring file))))))

(defun make-system-index (pathname)
  (with-open-file (stream (system-index-file pathname)
                          :direction :output
                          :if-exists :rename-and-delete)
    (dolist (system-file (local-project-system-files pathname))
      (write-line (native-namestring system-file) stream))
    (probe-file stream)))

(defun find-valid-system-index (pathname)
  (let* ((file (system-index-file pathname))
         (probed (probe-file file)))
    (when (and probed
               (<= (directory-write-date pathname)
                   (directory-write-date probed)))
      probed)))

(defun ensure-system-index (pathname)
  (or (find-valid-system-index pathname)
      (make-system-index pathname)))

(defun find-system-in-index (system index-file)
  (with-open-file (stream index-file)
    (loop for namestring = (read-line stream nil)
          while namestring
          when (string= system (pathname-name namestring))
          return (truename namestring))))

(defun local-projects-searcher (system-name)
  (when (probe-directory *local-projects-directory*)
    (let ((system-index (ensure-system-index *local-projects-directory*)))
      (when system-index
        (find-system-in-index system-name system-index)))))

(defun register-local-projects ()
  "Force a scan of the local projects directory to create the system
file index."
  (make-system-index *local-projects-directory*))
