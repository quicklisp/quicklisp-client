;;;; dist.lisp

(in-package #:ql-dist)


;;; Generic functions

(defgeneric dist (object)
  (:documentation
   "Return the dist of OBJECT."))

(defgeneric release (object)
  (:documentation
   "Return the release of OBJECT."))

(defgeneric system (object)
  (:documentation
   "Return the system of OBJECT."))

(defgeneric name (object)
  (:documentation
   "Return the name of OBJECT."))

(defgeneric find-system (name)
  (:documentation
   "Return a system with the given NAME, or NIL if no system is
   found. If multiple systems have the same name, the one with the
   highest preference is returned."))

(defgeneric find-release (name)
  (:documentation
   "Return a release with the given NAME, or NIL if no system is
   found. If multiple releases have the same name, the one with the
   highest preference is returned."))

(defgeneric find-systems-named (name)
  (:documentation
   "Return a list of all systems in all enabled dists with the given
   NAME, sorted by preference."))

(defgeneric find-releases-named (name)
  (:documentation
   "Return a list of all releases in all enabled dists with the given
   NAME, sorted by preference."))


(defgeneric base-directory (object)
  (:documentation
   "Return the base directory pathname of OBJECT."))

(defgeneric relative-to (object pathname)
  (:documentation
   "Merge PATHNAME with the base-directory of OBJECT.")
  (:method (object pathname)
    (merge-pathnames pathname (base-directory object))))


(defgeneric enabledp (object)
  (:documentation
   "Return true if OBJECT is enabled."))

(defgeneric enable (object)
  (:documentation
   "Enable OBJECT."))

(defgeneric disable (object)
  (:documentation
   "Disable OBJECT."))

(defgeneric installedp (object)
  (:documentation
   "Return true if OBJECT is installed."))

(defgeneric install (object)
  (:documentation
   "Install OBJECT."))

(defgeneric ensure-installed (object)
  (:documentation
   "Ensure that OBJECT is installed.")
  (:method (object)
    (unless (installedp object)
      (install object))
    object))

(defgeneric uninstall (object)
  (:documentation
   "Uninstall OBJECT."))

(defgeneric metadata-name (object)
  (:documentation
   "The metadata-name of an object is used to form the pathname for a
   few different object metadata files."))

(defgeneric install-metadata-file (object)
  (:documentation
   "The pathname to a file describing the installation status of
   OBJECT."))


(defgeneric preference-parent (object)
  (:documentation
   "Return a value suitable for checking if OBJECT has no specific
   preference set.")
  (:method (object)
    nil))

(defgeneric preference-file (object)
  (:documentation
   "Return the file from which preference information is loaded for
   OBJECT.")
  (:method (object)
    (relative-to object "preference.txt")))

(defgeneric preference (object)
  (:documentation
   "Returns a value used when comparing multiple systems or releases
   with the same name. Objects with higher preference are returned by
   FIND-SYSTEM and FIND-RELEASE.")
  (:method ((object null))
    0)
  (:method (object)
    (with-open-file (stream (preference-file object)
                            :if-does-not-exist nil)
      (if stream
          (values (parse-integer (read-line stream)))
          (preference (preference-parent object))))))

(defgeneric (setf preference) (preference object)
  (:documentation
   "Set the preference for OBJECT. Objects with higher preference are
   returned by FIND-SYSTEM and FIND-RELEASE.")
  (:method (preference object)
    (check-type preference integer)
    (let ((preference-file (preference-file object)))
      (ensure-directories-exist preference-file)
      (with-open-file (stream (preference-file object)
                              :direction :output
                              :if-exists :supersede)
        (format stream "~D" preference)))
    preference))

(defgeneric forget-preference (object)
  (:documentation
   "Remove specific preference information for OBJECT.")
  (:method (object)
    (delete-file-if-exists (preference-file object))))

(defgeneric short-description (object)
  (:documentation "Return a short string describing OBJECT."))


(defgeneric provided-releases (object)
  (:documentation "Return a list of releases provided by OBJECT."))

(defgeneric provided-systems (object)
  (:documentation "Return a list of systems provided by OBJECT."))

(defgeneric installed-releases (dist)
  (:documentation
   "Return a list of all releases installed for DIST.")
  (:method (dist)
    (remove-if-not #'installedp (provided-releases dist))))

(defgeneric installed-systems (dist)
  (:documentation
   "Return a list of all systems installed for DIST.")
  (:method (dist)
    (remove-if-not #'installedp (provided-systems dist))))

(defgeneric new-version-available-p (dist)
  (:documentation
   "Return true if a new version of DIST is available."))

(defgeneric find-system-in-dist (system-name dist)
  (:documentation
   "Return a system with the given NAME in DIST, or NIL if no system
   is found."))

(defgeneric find-release-in-dist (release-name dist)
  (:documentation
   "Return a release with the given NAME in DIST, or NIL if no release
   is found."))


(defgeneric ensure-system-index-file (dist)
  (:documentation
   "Return the pathname for the system index file of DIST, fetching it
   from a remote source first if necessary."))

(defgeneric ensure-release-index-file (dist)
  (:documentation
   "Return the pathname for the release index file of DIST, fetching
   it from a remote source first if necessary."))


(defgeneric initialize-release-index (dist)
  (:documentation
   "Initialize the release index of DIST."))

(defgeneric initialize-system-index (dist)
  (:documentation
   "Initialize the system index of DIST."))


(defgeneric local-archive-file (release)
  (:documentation
   "Return the pathname to where the archive file of RELEASE should be
   stored."))

(defgeneric ensure-local-archive-file (release)
  (:documentation
   "If the archive file for RELEASE is not available locally, fetch it
   and return the pathname to it."))

(defgeneric local-archive-file-valid-p (release)
  (:documentation
   "Check the local archive file of RELEASE for validity, including
   size and signature checks."))


(defgeneric archive-url (release)
  (:documentation
   "Return the full URL for fetching the archive file of RELEASE."))

(defgeneric installed-asdf-system-file (object)
  (:documentation
   "Return the path to the installed ASDF system file for OBJECT, or
   NIL if there is no installed system file."))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro destructure-line (lambda-list line &body body)
    `(destructuring-bind ,lambda-list
         (split-spaces ,line)
       ,@body))

  (defun call-for-each-line (fun file)
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line do (funcall fun line))))

  (defmacro for-each-line ((line file) &body body)
    `(call-for-each-line (lambda (,line) ,@body) ,file)))

(defun make-line-instance (line class &rest initargs)
  "Create an instance from words in an index file line. The last initarg collects all the trailing arguments, if any."
  (let* ((words (split-spaces line))
         (args (mapcan #'list
                       (butlast initargs)
                       words))
         (trailing (subseq words (1- (length initargs)))))
    (apply #'make-instance class (first (last initargs)) trailing args)))

(defun ignorable-line (line)
  (labels ((blank-char-p (char)
             (member char '(#\Space #\Tab)))
           (blankp (line)
             (every #'blank-char-p line))
           (ignorable (line)
             (or (zerop (length line))
                 (blankp line)
                 (eql (char line 0) #\#))))
    (ignorable line)))

(defvar *initarg-case-converter*
  (cond ((string= :string "string")
         #'string-downcase)
        ((string= :string "STRING")
         #'string-upcase)
        (t
         (error "Cannot determine default case of symbols."))))

(defun config-file-initargs (file)
  (flet ((initarg-keyword (string)
           ;; A concession to mlisp
           (intern (funcall *initarg-case-converter* string)
                   'keyword)))
    (let ((initargs '()))
      (for-each-line (line file)
        (unless (ignorable-line line)
          (destructure-line (initarg value)
              line
            (let ((keyword (initarg-keyword (string-right-trim ":" initarg))))
              (push value initargs)
              (push keyword initargs)))))
      initargs)))

;;;
;;; A few generic things
;;;

(defmethod dist ((name symbol))
  (dist (string name)))

(defmethod dist ((name string))
  (find-dist (string-downcase name)))

(defmethod release ((name symbol))
  (release (string name)))

(defmethod release ((name string))
  (find-release (string-downcase name)))

(defmethod system ((name symbol))
  (system (string name)))

(defmethod system ((name string))
  (find-system (string-downcase name)))

;;;
;;; Dists
;;;
;;; A dist is a set of releases.
;;;

(defclass dist ()
  ((base-directory
    :initarg :base-directory
    :accessor base-directory)
   (name
    :initarg :name
    :accessor name)
   (version
    :initarg :version
    :accessor version)
   (system-index-url
    :initarg :system-index-url
    :accessor system-index-url)
   (release-index-url
    :initarg :release-index-url
    :accessor release-index-url)
   (archive-base-url
    :initarg :archive-base-url
    :accessor archive-base-url)
   (canonical-distinfo-url
    :initarg :canonical-distinfo-url
    :accessor canonical-distinfo-url)
   (distinfo-subscription-url
    :initarg :distinfo-subscription-url
    :accessor distinfo-subscription-url)
   (system-index
    :initarg :system-index
    :accessor system-index)
   (release-index
    :initarg :release-index
    :accessor release-index)
   (local-distinfo-file
    :initarg :local-distinfo-file
    :accessor local-distinfo-file))
  (:default-initargs
   :name "unnamed"
   :version "unknown"))

(defmethod short-description ((dist dist))
  (format nil "~A ~A" (name dist) (version dist)))

(defmethod print-object ((dist dist) stream)
  (print-unreadable-object (dist stream :type t)
    (write-string (short-description dist) stream)))


(defmethod provided-releases ((dist dist))
  (loop for release being each hash-value of (release-index dist)
        collect release))

(defmethod provided-systems ((dist dist))
  (loop for system being each hash-value of (system-index dist)
        collect system))

(defmethod ensure-release-index-file ((dist dist))
  (let ((pathname (relative-to dist "releases.txt")))
    (or (probe-file pathname)
        (nth-value 1 (fetch (release-index-url dist) pathname)))))

(defmethod ensure-system-index-file ((dist dist))
  (let ((pathname (relative-to dist "systems.txt")))
    (or (probe-file pathname)
        (nth-value 1 (fetch (system-index-url dist) pathname)))))


(defun dist-name-pathname (name)
  "Return the pathname that would be used for an installed dist with
the given NAME."
  (qmerge (make-pathname :directory (list :relative "dists" name))))

(defmethod slot-unbound (class (dist dist) (slot (eql 'base-directory)))
  (setf (base-directory dist) (dist-name-pathname (name dist))))

(defun make-dist-from-file (file)
  "Load dist info from FILE and use it to create a dist instance."
  (let ((initargs (config-file-initargs file)))
    (apply #'make-instance 'dist :local-distinfo-file file initargs)))

(defmethod install-metadata-file ((dist dist))
  (relative-to dist "distinfo.txt"))

(defun find-dist (name)
  (let ((pathname (merge-pathnames "distinfo.txt"
                                   (dist-name-pathname name))))
    (when (probe-file pathname)
      (make-dist-from-file pathname))))

(defmethod enabledp ((dist dist))
  (not (not (probe-file (relative-to dist "enabled.txt")))))

(defmethod enable ((dist dist))
  (ensure-file-exists (relative-to dist "enabled.txt"))
  t)

(defmethod disable ((dist dist))
  (delete-file-if-exists (relative-to dist "enabled.txt"))
  t)

(defmethod installedp ((dist dist))
  (let ((installed (find-dist (name dist))))
    (equalp (version installed) (version dist))))


(defmethod find-release-in-dist (release dist)
  (values (gethash release (release-index dist))))


(defparameter *dist-enumeration-functions*
  '(standard-dist-enumeration-function)
  "ALL-DISTS calls each function in this list with no arguments, and
  appends the results into a list of dist objects, removing
  duplicates. Functions might be called just once for a batch of
  related operations; see WITH-CONSISTENT-DISTS.")

(defun standard-dist-enumeration-function ()
  "The default function used for producing a list of dist objects."
  (loop for file in (directory (qmerge "dists/*/distinfo.txt"))
        collect (make-dist-from-file file)))

(defun all-dists ()
  "Return a list of all known dists."
  (remove-duplicates
   (apply 'append (mapcar 'funcall *dist-enumeration-functions*))))

(defun enabled-dists ()
  "Return a list of all known dists for which ENABLEDP returns true."
  (remove-if-not #'enabledp (all-dists)))


(defmethod install-metadata-file (object)
  (relative-to (dist object)
               (make-pathname :directory
                              (list :relative "installed"
                                    (metadata-name object))
                              :name (name object)
                              :type "txt")))


(defclass preference-mixin () ()
  (:documentation
   "Instances of this class have a special location for their
   preference files."))

(defmethod preference-file ((object preference-mixin))
  (relative-to
   (dist object)
   (make-pathname :directory (list :relative
                                   "preferences"
                                   (metadata-name object))
                              :name (name object)
                              :type "txt")))


;;;
;;; Releases
;;;

(defclass release (preference-mixin)
  ((project-name
    :initarg :project-name
    :accessor name
    :accessor project-name)
   (dist
    :initarg :dist
    :accessor dist
    :reader preference-parent)
   (provided-systems
    :initarg :provided-systems
    :accessor provided-systems)
   (archive-url
    :initarg :archive-url
    :accessor archive-url)
   (archive-size
    :initarg :archive-size
    :accessor archive-size)
   (archive-md5
    :initarg :archive-md5
    :accessor archive-md5)
   (archive-content-sha1
    :initarg :archive-content-sha1
    :accessor archive-content-sha1)
   (prefix
    :initarg :prefix
    :accessor prefix
    :reader short-description)
   (system-files
    :initarg :system-files
    :accessor system-files)
   (metadata-name
    :initarg :metadata-name
    :accessor metadata-name))
  (:default-initargs
   :metadata-name "releases")
  (:documentation
   "Instances of this class represent a snapshot of a project at some
   point in time, which might be from version control, or from an
   official release, or from some other source."))

(defmethod print-object ((release release) stream)
  (print-unreadable-object (release stream :type t)
    (format stream "~A / ~A"
            (short-description release)
            (short-description (dist release)))))

(defmethod local-archive-file-valid-p ((release release))
  t)

(defmethod local-archive-file ((release release))
  (relative-to (dist release)
               (make-pathname :directory '(:relative "archives")
                              :defaults (file-namestring
                                         (path (url (archive-url release)))))))

(defmethod ensure-local-archive-file ((release release))
  (let ((pathname (local-archive-file release)))
    (or (probe-file pathname)
        (progn
          (ensure-directories-exist pathname)
          (nth-value 1 (fetch (archive-url release) pathname))))))


(defmethod base-directory ((release release))
  (relative-to
   (dist release)
   (make-pathname :directory (list :relative "software" (prefix release)))))

(defmethod installedp ((release release))
  (and (probe-file (install-metadata-file release))
       (every #'installedp (provided-systems release))))

(defmethod install ((release release))
  (let ((archive (ensure-local-archive-file release))
        (tar (qmerge "tmp/release-install.tar"))
        (output (relative-to (dist release)
                             (make-pathname :directory
                                            (list :relative "software"))))
        (tracking (install-metadata-file release)))
    (ensure-directories-exist tar)
    (ensure-directories-exist output)
    (ensure-directories-exist tracking)
    (gunzip archive tar)
    (unpack-tarball tar :directory output)
    (ensure-directories-exist tracking)
    (with-open-file (stream tracking
                            :direction :output
                            :if-exists :supersede)
      (write-line (qenough (base-directory release)) stream))
    (let ((provided (provided-systems release))
          (dist (dist release)))
      (dolist (file (system-files release))
        (let ((system (find-system-in-dist (pathname-name file) dist)))
          (unless (member system provided)
            (error "FIND-SYSTEM-IN-DIST returned ~A but I expected one of ~A"
                   system provided))
          (let ((system-tracking (install-metadata-file system))
                (system-file (merge-pathnames file
                                              (base-directory release))))
            (ensure-directories-exist system-tracking)
            (unless (probe-file system-file)
              (error "Release claims to have ~A, but I can't find it"
                     system-file))
            (with-open-file (stream system-tracking
                                    :direction :output
                                    :if-exists :supersede)
              (write-line (qenough system-file)
                          stream))))))
    release))


(defun call-for-each-index-entry (file fun)
  (labels ((blank-char-p (char)
             (member char '(#\Space #\Tab)))
           (blankp (line)
             (every #'blank-char-p line))
           (ignorable (line)
             (or (zerop (length line))
                 (blankp line)
                 (eql (char line 0) #\#))))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line do
            (unless (ignorable line)
              (funcall fun line))))))

(defmethod initialize-release-index ((dist dist))
  (let ((releases (ensure-release-index-file dist))
        (index (make-hash-table :test 'equal)))
    (call-for-each-index-entry
     releases
     (lambda (line)
       (let ((instance (make-line-instance line 'release
                                           :project-name
                                           :archive-url
                                           :archive-size
                                           :archive-md5
                                           :archive-content-sha1
                                           :prefix
                                           :system-files)))
         (setf (dist instance) dist)
         (setf (archive-size instance) (parse-integer (archive-size instance)))
         (setf (gethash (project-name instance) index) instance))))
    (setf (release-index dist) index)))

(defmethod slot-unbound (class (dist dist) (slot (eql 'release-index)))
  (initialize-release-index dist))


;;;
;;; Systems
;;;
;;; A "system" in the defsystem sense.
;;;

(defclass system (preference-mixin)
  ((name
    :initarg :name
    :accessor name
    :reader short-description)
   (system-file-name
    :initarg :system-file-name
    :accessor system-file-name)
   (release
    :initarg :release
    :accessor release
    :reader preference-parent)
   (dist
    :initarg :dist
    :accessor dist)
   (required-systems
    :initarg :required-systems
    :accessor required-systems)
   (metadata-name
    :initarg :metadata-name
    :accessor metadata-name))
  (:default-initargs
   :metadata-name "systems"))

(defmethod print-object ((system system) stream)
  (print-unreadable-object (system stream :type t)
    (format stream "~A / ~A / ~A"
            (short-description system)
            (short-description (release system))
            (short-description (dist system)))))

(defmethod provided-systems ((system system))
  (list system))

(defmethod initialize-system-index ((dist dist))
  (let ((systems (ensure-system-index-file dist))
        (index (make-hash-table :test 'equal)))
    (call-for-each-index-entry
     systems
     (lambda (line)
       (let ((instance (make-line-instance line 'system
                                           :release
                                           :system-file-name
                                           :name
                                           :required-systems)))
         (let ((release (find-release-in-dist (release instance) dist)))
           (setf (release instance) release)
           (if (slot-boundp release 'provided-systems)
               (pushnew instance (provided-systems release))
               (setf (provided-systems release) (list instance))))
         (setf (dist instance) dist)
         (setf (gethash (name instance) index) instance))))
    (setf (system-index dist) index)))

(defmethod slot-unbound (class (release release) (slot (eql 'provided-systems)))
  (initialize-system-index (dist release))
  (if (slot-boundp release 'provided-systems)
      (provided-systems release)
      (setf (provided-systems release) nil)))

(defmethod slot-unbound (class (dist dist) (slot (eql 'system-index)))
  (initialize-system-index dist))

(defmethod find-system-in-dist (system-name dist)
  (values (gethash system-name (system-index dist))))

(defmethod preference ((system system))
  (if (probe-file (preference-file system))
      (call-next-method)
      (preference (release system))))

(defun find-thing-named (find-fun name)
  (let ((result '()))
    (dolist (dist (enabled-dists) (sort result #'> :key #'preference))
      (let ((thing (funcall find-fun name dist)))
        (when thing
          (push thing result))))))

(defmethod find-systems-named (name)
  (find-thing-named #'find-system-in-dist name))

(defmethod find-releases-named (name)
  (find-thing-named #'find-release-in-dist name))

(defmethod find-system (name)
  (first (find-systems-named name)))

(defmethod find-release (name)
  (first (find-releases-named name)))

(defmethod install ((system system))
  (ensure-installed (release system)))


(defmethod install-metadata-file ((system system))
  (relative-to (dist system)
               (make-pathname :name (system-file-name system)
                              :type "txt"
                              :directory '(:relative "installed" "systems"))))

(defmethod installed-asdf-system-file ((system system))
  (let ((metadata-file (install-metadata-file system)))
    (when (probe-file metadata-file)
      (with-open-file (stream metadata-file)
        (let* ((relative (read-line stream))
               (full (qmerge relative)))
          (when (probe-file full)
            full))))))

(defmethod installedp ((system system))
  (installed-asdf-system-file system))

(defun find-asdf-system-file (name)
  (let ((system (find-system name)))
    (when system
      (installed-asdf-system-file system))))

(defun call-with-consistent-dists (fun)
  "Take a snapshot of the available dists and return the same list
consistently each time ALL-DISTS is called in the dynamic scope of
FUN."
  (let* ((all-dists (all-dists))
         (*dist-enumeration-functions* (list (constantly all-dists))))
    (funcall fun)))

(defmacro with-consistent-dists (&body body)
  "See CALL-WITH-CONSISTENT-DISTS."
  `(call-with-consistent-dists (lambda () ,@body)))


(defgeneric dependency-tree (system)
  (:method ((string string))
    (dependency-tree (find-system string)))
  (:method ((system system))
    (with-consistent-dists
      (list* system (mapcar 'dependency-tree (required-systems system))))))


(defmethod provided-systems ((object (eql t)))
  (let ((systems (loop for dist in (all-dists)
                       appending (provided-systems dist))))
    (sort systems #'string< :key #'name)))


(defgeneric system-apropos (term)
  (:method ((term string))
    (dolist (system (provided-systems t))
      (when (search term (name system))
        (format t "~A~%" system)))))
