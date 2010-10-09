;;;; client.lisp

(in-package #:quicklisp-client)

(defvar *quickload-verbose* nil)
(defvar *quickload-prompt* nil)
(defvar *quickload-explain* t)

(define-condition system-not-quickloadable (error)
  ((system
    :initarg :system
    :reader not-quickloadable-system)))

(defgeneric quickload (systems &key verbose prompt explain &allow-other-keys)
  (:documentation
   "Load SYSTEMS the quicklisp way. SYSTEMS is a designator for a list
   of things to be loaded.")
  (:method (systems &key prompt verbose &allow-other-keys)
    (unless (consp systems)
      (setf systems (list systems)))
    (dolist (thing systems systems)
      (flet ((ql ()
               (autoload-system-and-dependencies thing :prompt prompt)))
        (if verbose
            (ql)
            (call-with-quiet-compilation #'ql))))))

(defun system-list ()
  (provided-systems t))

(defun update-dist (dist &key (prompt t))
  (when (stringp dist)
    (setf dist (find-dist dist)))
  (let ((new (available-update dist)))
    (cond (new
           (show-update-report dist new)
           (when (or (not prompt) (press-enter-to-continue))
             (update-in-place dist new)))
          (t
           (format t "~&No update available for ~S."
                   (short-description dist))))))

(defun update-all-dists (&key (prompt t))
  (dolist (old (all-dists))
    (with-simple-restart (skip "Skip update of dist ~S" (name old))
      (update-dist old :prompt prompt))))

(defun help ()
  "For help with this demo, see http://www.quicklisp.org/demo/")
