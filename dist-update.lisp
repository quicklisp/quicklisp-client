;;;; dist-update.lisp

(in-package #:ql-dist)

(defgeneric available-update (dist)
  (:documentation "If an update is available for DIST, return the
  update as an uninstalled dist object. Otherwise, return NIL."))

(defgeneric update-release-differences (old-dist new-dist)
  (:documentation "Compare OLD-DIST to NEW-DIST and return three lists
  as multiple values: new releases \(present in NEW-DIST but not
  OLD-DIST), changed releases \(present in both dists but different in
  some way), and removed releases \(present in OLD-DIST but not
  NEW-DIST). The list of changed releases is a list of two-element
  lists, with each two-element list having first the old release
  object and then the new release object."))

(defgeneric show-update-report (old-dist new-dist)
  (:documentation "Display a description of the update from OLD-DIST
  to NEW-DIST."))

(defgeneric update-in-place (old-dist new-dist)
  (:documentation "Update OLD-DIST to NEW-DIST in place."))

(defmethod available-update ((dist dist))
  (let ((url (distinfo-subscription-url dist))
        (target (qmerge "tmp/distinfo-update/distinfo.txt")))
    (when url
      (ensure-directories-exist target)
      (fetch url target :quietly t)
      (let ((new (make-dist-from-file target)))
        (setf (base-directory new) (directory-namestring target))
        (when (and (string= (name dist) (name new))
                   (string/= (version dist) (version new)))
          new)))))

(defmethod update-release-differences ((old-dist dist)
                                       (new-dist dist))
  (let ((old-releases (provided-releases old-dist))
        (new-releases (provided-releases new-dist))
        (new '())
        (updated '())
        (removed '())
        (old-by-name (make-hash-table :test 'equalp)))
    (dolist (release old-releases)
      (setf (gethash (name release) old-by-name)
            release))
    (dolist (new-release new-releases)
      (let* ((name (name new-release))
             (old-release (gethash name old-by-name)))
        (remhash name old-by-name)
        (cond ((not old-release)
               (push new-release new))
              ((not (equal (archive-content-sha1 new-release)
                           (archive-content-sha1 old-release)))
               (push (list old-release new-release) updated)))))
    (maphash (lambda (name old-release)
               (declare (ignore name))
               (push old-release removed))
             old-by-name)
    (values (nreverse  new)
            (nreverse  updated)
            (sort removed #'string< :key #'prefix))))

(defmethod show-update-report ((old-dist dist) (new-dist dist))
  (multiple-value-bind (new updated removed)
      (update-release-differences old-dist new-dist)
    (format t "Changes from ~A ~A to ~A ~A:~%"
            (name old-dist)
            (version old-dist)
            (name new-dist)
            (version new-dist))
    (when new
      (format t "~&  New projects:~%")
      (format t "~{    ~A~%~}" (mapcar #'prefix new)))
    (when updated
      (format t "~%  Updated projects:~%")
      (loop for (old-release new-release) in updated
            do (format t "    ~A -> ~A~%"
                       (prefix old-release)
                       (prefix new-release))))
    (when removed
      (format t "~%  Removed projects:~%")
      (format t "~{    ~A~%~}" (mapcar #'prefix removed)))))

(defmethod update-in-place :after ((old-dist dist) (new-dist dist))
  ;; XXX Is this too nasty?
  (clrhash asdf::*defined-systems*))

(defmethod update-in-place ((old-dist dist) (new-dist dist))
  (flet ((remove-installed (type)
           (let ((wild (merge-pathnames (make-pathname :directory
                                                       (list :relative
                                                             "installed"
                                                             type)
                                                       :name :wild
                                                       :type "txt")
                                        (base-directory old-dist))))
             (dolist (file (directory wild))
               (delete-file file)))))
    (let ((reinstall-releases (installed-releases old-dist)))
      (remove-installed "systems")
      (remove-installed "releases")
      (delete-file-if-exists (relative-to old-dist "releases.txt"))
      (delete-file-if-exists (relative-to old-dist "systems.txt"))
      (replace-file (local-distinfo-file new-dist)
                    (local-distinfo-file old-dist))
      (setf new-dist (find-dist (name new-dist)))
      (dolist (old-release reinstall-releases)
        (let* ((name (name old-release))
               (new-release (find-release-in-dist name new-dist)))
          (if new-release
              (ensure-installed new-release)
              (warn "~S is not available in ~A" name new-dist)))))))


