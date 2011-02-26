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
  (:implementation cmucl
    (rename-file from (string-right-trim "/" (namestring to)))
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
    #+windows
    "ccl-init.lisp"
    #-windows
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
  (let ((*print-case* :downcase))
    (format stream "~v@T(let ((quicklisp-init ~S))~%"
            indentation
            (quicklisp-init-file-form)))
  (format stream "~v@T  (when (probe-file quicklisp-init)~%" indentation)
  (format stream "~v@T    (load quicklisp-init)))~%~%" indentation))

(defun suitable-lisp-init-file (implementation)
  "Return the name of IMPLEMENTATION's init file. If IMPLEMENTAION is
a string or pathname, return its merged pathname instead."
  (etypecase implementation
    ((or string pathname)
     (merge-pathnames implementation))
    ((or null (eql t))
     (init-file-name))
    (t
     (init-file-name-for implementation))))

(defun add-to-init-file (&optional implementation-or-file)
  "Add forms to the Lisp implementation's init file that will load
quicklisp at CL startup."
  (let ((init-file (suitable-lisp-init-file implementation-or-file)))
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



;;;
;;; Deleting a directory tree
;;;

(defvar *wild-entry* (make-pathname :name :wild
                                    :type :wild
                                    :version :wild))

(defvar *wild-relative* (make-pathname :directory '(:relative :wild)))

(defun wild-all-entries (directory)
  (directory (merge-pathnames *wild-entry* directory)))

(defun clisp-style-directory-entries (directory)
  (nconc (wild-all-entries directory)
         (directory (merge-pathnames *wild-relative* directory))))

(definterface directory-entries (directory)
  (:documentation "Return all directory entries of DIRECTORY as a
  list, or NIL if there are no directory entries. Excludes the \".\"
  and \"..\" entries.")
  (:implementation t
     ;; Argh. I hate #+/#-, but without it SBCL gives a full warning
     ;; about unknown keyword arguments to DIRECTORY in 1.0.46 and
     ;; older.
     (directory (merge-pathnames *wild-entry* directory)
                #+ccl :directories #+ccl t
                ))
  (:implementation (clisp ecl)
     (clisp-style-directory-entries directory)))

(definterface directoryp (entry)
  (:documentation "Return true if ENTRY refers to a directory.")
  (:implementation t
     (not (or (pathname-name entry)
              (pathname-type entry)
              (pathname-version entry))))
  (:implementation allegro
     (ql-allegro:file-directory-p entry))
  (:implementation ecl
     (eql (ql-ecl:file-kind entry nil) :directory))
  (:implementation lispworks
     (ql-lispworks:file-directory-p entry)))

(definterface delete-directory (entry)
  (:documentation "Delete the directory ENTRY. Might signal an error
  if it is not an empty directory.")
  (:implementation t
     (delete-file entry))
  (:implementation allegro
     (ql-allegro:delete-directory entry))
  (:implementation ccl
     (ql-ccl:delete-directory entry))
  (:implementation clisp
     (ql-clisp:delete-dir entry))
  (:implementation cmucl
     (ql-cmucl:unix-rmdir (namestring entry)))
  (:implementation ecl
     (ql-ecl:rmdir entry))
  (:implementation lispworks
     (ql-lispworks:delete-directory entry))
  (:implementation sbcl
     (ql-sbcl:rmdir entry)))

(definterface delete-directory-tree (pathname)
  (:documentation "Delete the directory tree rooted at PATHNAME.")
  (:implementation t
     (let ((directories-to-process (list (truename pathname)))
           (directories-to-delete '()))
         (loop
           (unless directories-to-process
             (return))
           (let* ((current (pop directories-to-process))
                  (entries (directory-entries current)))
             (push current directories-to-delete)
             (dolist (entry entries)
               (if (directoryp entry)
                   (push entry directories-to-process)
                   (delete-file entry)))))
         (map nil 'delete-directory directories-to-delete)))
  (:implementation allegro
     (ql-allegro:delete-directory-and-files pathname)))
