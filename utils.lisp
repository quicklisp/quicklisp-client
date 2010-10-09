;;;; utils.lisp

(in-package #:ql-util)

(defun write-line-to-file (string file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede)
    (write-line string stream)))

(defun press-enter-to-continue ()
  (format *query-io* "~&Press Enter to continue.~%")
  (let ((result (read-line *query-io*)))
    (zerop (length result))))

(defun replace-file (from to)
  "Like RENAME-FILE, but deletes TO if it exists, first."
  (when (probe-file to)
    (delete-file to))
  (rename-file from to))

(defun ensure-file-exists (pathname)
  (open pathname :direction :probe :if-does-not-exist :create))

(defun delete-file-if-exists (pathname)
  (when (probe-file pathname)
    (delete-file pathname)))

(defun split-spaces (line)
  (let ((words '())
        (mark 0)
        (pos 0))
    (labels ((finish ()
               (setf pos (length line))
               (save)
               (return-from split-spaces (nreverse words)))
             (save ()
               (when (< mark pos)
                 (push (subseq line mark pos) words)))
             (mark ()
               (setf mark pos))
             (in-word (char)
               (case char
                 (#\Space
                    (save)
                    #'in-space)
                 (t
                    #'in-word)))
             (in-space (char)
               (case char
                 (#\Space
                    #'in-space)
                 (t
                    (mark)
                    #'in-word))))
      (let ((state #'in-word))
        (dotimes (i (length line) (finish))
          (setf pos i)
          (setf state (funcall state (char line i))))))))

(defun first-line (file)
  (with-open-file (stream file)
    (values (read-line stream))))

(defun (setf first-line) (line file)
  (with-open-file (stream file :direction :output
                          :if-exists :rename-and-delete)
    (write-line line stream)))
