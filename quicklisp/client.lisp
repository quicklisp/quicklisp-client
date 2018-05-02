;;;; client.lisp

(in-package #:quicklisp-client)

(defvar *quickload-verbose* nil
  "When NIL, show terse output when quickloading a system. Otherwise,
  show normal compile and load output.")

(defvar *quickload-prompt* nil
  "When NIL, quickload systems without prompting for enter to
  continue, otherwise proceed directly without user intervention.")

(defvar *quickload-explain* t)

(define-condition system-not-quickloadable (error)
  ((system
    :initarg :system
    :reader not-quickloadable-system)))

(defun maybe-silence (silent stream)
  (or (and silent (make-broadcast-stream)) stream))

(defgeneric quickload (systems &key verbose silent prompt explain &allow-other-keys)
  (:documentation
   "Load SYSTEMS the quicklisp way. SYSTEMS is a designator for a list
   of things to be loaded.")
  (:method (systems &key
            (prompt *quickload-prompt*)
            (silent nil)
            (verbose *quickload-verbose*) &allow-other-keys)
    (let ((*standard-output* (maybe-silence silent *standard-output*))
          (*trace-output*    (maybe-silence silent *trace-output*)))
      (unless (consp systems)
        (setf systems (list systems)))
      (dolist (thing systems systems)
        (flet ((ql ()
                 (autoload-system-and-dependencies thing :prompt prompt)))
          (if verbose
              (ql)
              (call-with-quiet-compilation #'ql)))))))

(defmethod quickload :around (systems &key verbose prompt explain
                                      &allow-other-keys)
  (declare (ignorable systems verbose prompt explain))
  (with-consistent-dists
    (call-next-method)))

(defun system-list ()
  (provided-systems t))

(defun all-installed-systems ()
  (remove-if-not #'installedp (system-list)))

(defun releases-included-by (system)
  (mapcar #'release (flatten (dependency-tree system))))

(defun update-dist (dist &key (prompt t))
  (when (stringp dist)
    (setf dist (find-dist dist)))
  (let ((new (available-update dist)))
    (cond (new
           (show-update-report dist new)
           (when (or (not prompt) (press-enter-to-continue))
             (update-in-place dist new)))
          ((not (subscribedp dist))
           (format t "~&You are not subscribed to ~S."
                   (name dist)))
          (t
           (format t "~&You already have the latest version of ~S: ~A.~%"
                   (name dist)
                   (version dist))))))

(defun update-all-dists (&key (prompt t))
  (let ((dists (remove-if-not 'subscribedp (all-dists))))
    (format t "~&~D dist~:P to check.~%" (length dists))
    (dolist (old dists)
      (with-simple-restart (skip "Skip update of dist ~S" (name old))
        (update-dist old :prompt prompt)))))

(defun available-dist-versions (name)
  (available-versions (find-dist-or-lose name)))

(defun help ()
  "For help with Quicklisp, see http://www.quicklisp.org/beta/")

(defun uninstall (systems &key remove-dependencies (prompt t))
  "uninstalls the system(s) from quicklisp. 
   When remove-dependencies is specified, all dependencies of the system are also
   removed when they are not required by another system.
   You are prompted before uninstalling each dependency unless prompt is set to nil."
  (unless (consp systems)
    (setf systems (list systems)))
  (let ((uninstalled-systems nil))
    (dolist (system-name systems uninstalled-systems)
      (let ((system (find-system system-name)))
        (cond ((and system remove-dependencies)
               (mapcar #'(lambda (sys)
                           (when (or (not prompt) (y-or-n-p "Uninstall ~S?~%" sys))
                             (push sys uninstalled-systems)
                             (uninstall sys)))
                       (removable-system-dependencies system-name)))
              (system
               (ql-dist:uninstall system)
               (push system uninstalled-systems))
              (t
               (warn "Unknown system ~S" system-name)
               nil))))))

(defun all-releases (ignore-set)
  (apply #'append
         (mapcar #'(lambda (system)
                     (releases-included-by system))
                 (remove-if
                  #'(lambda (system)
                      (member (name (release system)) ignore-set :test #'string=))
                  (all-installed-systems)))))

(defun removable-system-dependencies (system-name)
  "Returns a list of safely removable dependencies of system-name."
  (when (symbolp system-name)
    (setf system-name (string-downcase (symbol-name system-name))))
  (let ((system (find-system system-name)))
    (cond ((not system)
           (warn "Unknown system ~S~%" system-name))
          ((not (installedp system))
           (warn "System ~S is not installed" system-name))
          (t
           ;; consider the set X of system dependencies
           ;; if any piece of installed software S depends on a member of X
           ;; and is not itself a member of X, then we cannot delete S
           (let* ((system-releases (mapcar #'name (releases-included-by system)))
                  (all-other-dependencies
                   (mapcar #'name (all-releases system-releases)))
                  (to-remove (list system-name)))
             (dolist (release system-releases (remove-duplicates to-remove :test #'string=))
               (unless (member release all-other-dependencies :test #'string=)
                 (push release to-remove))))))))

(defun uninstall-dist (name)
  (let ((dist (find-dist name)))
    (when dist
      (ql-dist:uninstall dist))))

(defun write-asdf-manifest-file (output-file &key (if-exists :rename-and-delete)
                                               exclude-local-projects)
  "Write a list of system file pathnames to OUTPUT-FILE, one per line,
in order of descending QL-DIST:PREFERENCE."
  (when (or (eql output-file nil)
            (eql output-file t))
    (setf output-file (qmerge "manifest.txt")))
  (with-open-file (stream output-file
                          :direction :output
                          :if-exists if-exists)
    (unless exclude-local-projects
      (register-local-projects)
      (dolist (system-file (list-local-projects))
        (let* ((enough (enough-namestring system-file output-file))
               (native (native-namestring enough)))
          (write-line native stream))))
    (with-consistent-dists
      (let ((systems (provided-systems t))
            (already-seen (make-hash-table :test 'equal)))
        (dolist (system (sort systems #'>
                              :key #'preference))
          ;; FIXME: find-asdf-system-file does another find-system
          ;; behind the scenes. Bogus. Should be a better way to go
          ;; from system object to system file.
          (let* ((system-file (find-asdf-system-file (name system)))
                 (enough (and system-file (enough-namestring system-file
                                                             output-file)))
                 (native (and enough (native-namestring enough))))
            (when (and native (not (gethash native already-seen)))
              (setf (gethash native already-seen) native)
              (format stream "~A~%" native)))))))
  (probe-file output-file))

(defun where-is-system (name)
  "Return the pathname to the source directory of ASDF system with the
given NAME, or NIL if no system by that name can be found known."
  (let ((system (asdf:find-system name nil)))
    (when system
      (asdf:system-source-directory system))))
