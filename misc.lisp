;;;; misc.lisp

(in-package #:quicklisp-client)

;;;
;;; This stuff will probably end up somewhere else.
;;;

(defun use-only-quicklisp-systems ()
  (asdf:initialize-source-registry
   '(:source-registry :ignore-inherited-configuration))
  (asdf:map-systems 'asdf:clear-system)
  t)


