":" #|
exec sbcl --noinform --no-userinit --no-sysinit --non-interactive --load "$0"
|#

;;;; coverage.lisp

(require 'asdf)
(require 'sb-cover)

(defun make-system-file-matcher (system-name)
  "Return a function of one argument that returns true if its argument
  is the pathname of a source component in SYSTEM."
  (let ((system (asdf:find-system system-name))
        (table (make-hash-table :test 'equalp)))
    (labels ((walk-component (component)
               (typecase component
                 (asdf:parent-component
                  (map nil #'walk-component (asdf:component-children component)))
                 (asdf:source-file
                  (let ((path (asdf:component-pathname component)))
                    (setf (gethash path table) path))))))
      (walk-component system)
      (lambda (file)
        (not (null (gethash (pathname file) table)))))))

(load "../setup.lisp")

(asdf::load-asd (merge-pathnames "quicklisp-client-tests.asd"
                                 *load-truename*))

(asdf:load-system "quicklisp-client-tests")

(declaim (optimize sb-cover:store-coverage-data))
(asdf:load-system "quicklisp" :force t)

(fiasco:run-package-tests :package :quicklisp-client-tests)
(let ((base (make-pathname :type nil :name nil
                           :defaults *load-truename*)))
  (sb-cover:report (merge-pathnames "coverage-report/"
                                    base)
                   :if-matches (make-system-file-matcher "quicklisp")))

