;;;; local-projects.lisp

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
