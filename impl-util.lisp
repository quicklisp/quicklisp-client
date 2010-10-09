;;;; impl-util.lisp

(in-package #:ql-impl-util)

(definterface call-with-quiet-compilation (fun)
  (:implementation t
    (let ((*load-verbose* nil)
          (*compile-verbose* nil)
          (*load-print* nil)
          (*compile-print* nil))
      (handler-bind ((warning #'muffle-warning))
        (funcall fun)))))

(defimplementation (call-with-quiet-compilation :for sbcl :qualifier :around)
    (fun)
  (handler-bind ((ql-sbcl:compiler-note #'muffle-warning))
    (call-next-method)))

(defimplementation (call-with-quiet-compilation :for cmucl :qualifier :around)
    (fun)
  (let ((ql-cmucl:*gc-verbose* nil))
    (call-next-method)))

(definterface rename-directory (from to)
  (:implementation t
    (rename-file from to)
    (truename to))
  (:implementation clisp
    (ql-clisp:rename-directory from to)
    (truename to)))


(definterface init-file-name ()
  (:implementation allegro
    ".clinit.cl")
  (:implementation abcl
    ".abclrc")
  (:implementation ccl
    ".ccl-init.lisp")
  (:implementation clisp
    ".clisprc.lisp")
  (:implementation ecl
    ".eclrc")
  (:implementation lispworks
    ".lispworks")
  (:implementation sbcl
    ".sbclrc")
  (:implementation cmucl
    ".cmucl-init.lisp"))

(defun init-file-name-for (&optional implementation-designator)
  (let* ((class-name (find-symbol (string-upcase implementation-designator)
                                  'ql-impl))
         (class (find-class class-name nil)))
    (when class
      (let ((*implementation* (make-instance class)))
        (init-file-name)))))

(defun quicklisp-init-file-form ()
  "Return a form suitable for describing the location of the quicklisp
  init file. If the file is available relative to the home directory,
  returns a form that merges with the home directory instead of
  specifying an absolute file."
  (let* ((init-file (ql-setup:qmerge "setup.lisp"))
         (enough (enough-namestring init-file (user-homedir-pathname))))
    (cond ((equal (pathname enough) (pathname init-file))
           ;; The init-file is somewhere outside of the home directory
           (pathname enough))
          (t
           `(merge-pathnames ,enough (user-homedir-pathname))))))

(defun write-init-forms (stream &key (indentation 0))
  (format stream "~%~v@T;;; The following lines added by ql:add-to-init-file:~%"
          indentation)
  (format stream "~v@T#-quicklisp~%" indentation)
  (format stream "~v@T(let ((quicklisp-init ~(~S~)))~%"
          indentation
          (quicklisp-init-file-form))
  (format stream "~v@T  (when (probe-file quicklisp-init)~%" indentation)
  (format stream "~v@T    (load quicklisp-init)))~%~%" indentation))

(defun suitable-lisp-init-file (implementation)
  "Return the name of IMPLEMENTATION's init file."
  (if implementation
      (init-file-name-for implementation)
      (init-file-name)))

(defun add-to-init-file (&optional implementation)
  "Add forms to the Lisp implementation's init file that will load
quicklisp at CL startup."
  (let ((init-file (suitable-lisp-init-file implementation)))
    (unless init-file
      (error "Don't know how to add to init file for your implementation."))
    (setf init-file (merge-pathnames init-file (user-homedir-pathname)))
    (format *query-io* "~&I will append the following lines to ~S:~%"
            init-file)
    (write-init-forms *query-io* :indentation 2)
    (when (ql-util:press-enter-to-continue)
      (with-open-file (stream init-file
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :append)
        (write-init-forms stream)))
    init-file))



