;;;; hashes/hashes.lisp

(cl:in-package #:ql-hashes)

(defun byte-vector-to-hex-string (seq)
  "Convert a vector of bytes to a hexidecimal string."
  (check-type seq (array (unsigned-byte 8) 1))
  (string-downcase (apply #'concatenate
                          (cons 'string
                                (loop for i across seq
                                   collect (format nil "~2,'0:X" i))))))


;;;; Code moved over from md5.lisp, which was originally copied from the MD5
;;;; package (https://github.com/pmai/md5/blob/master/md5.lisp) with some
;;;; changes.

#+cmu
(eval-when (:compile-toplevel)
  (defparameter *old-expansion-limit* ext:*inline-expansion-limit*)
  (setq ext:*inline-expansion-limit* (max ext:*inline-expansion-limit* 1000)))

#+cmu
(eval-when (:compile-toplevel :execute)
  (defparameter *old-features* *features*)
  (pushnew (c:backend-byte-order c:*target-backend*) *features*))

#+sbcl
(eval-when (:compile-toplevel :execute)
  (defparameter *old-features* *features*)
  (pushnew sb-c:*backend-byte-order* *features*))

#+(and :lispworks (or (not :lispworks4) :lispworks4.4))
(eval-when (:compile-toplevel :execute)
  (defparameter *old-features* *features*)
  (pushnew :lw-int32 *features*)
  (defmacro lw-int32-no-overflow (value)
    ;; Prevent overflow in 64-bit prior to LispWorks 7.0.
    #+(and :lispworks-64bit (or :lispworks5 :lispworks6))
    `(sys:int32>> (sys:int32<< ,value #.(sys:integer-to-int32 32))
                  #.(sys:integer-to-int32 32))
    #-(and :lispworks-64bit (or :lispworks5 :lispworks6))
    value))

;;; Section 2:  Basic Datatypes

(deftype ub32 ()
  "Corresponds to the 32bit quantity word of the MD5 Spec"
  #+lw-int32 'sys:int32
  #-lw-int32 '(unsigned-byte 32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro assemble-ub32 (a b c d)
    "Assemble an ub32 value from the given (unsigned-byte 8) values,
where a is the intended low-order byte and d the high-order byte."
    #+lw-int32
    `(lw-int32-no-overflow
      (sys:int32-logior (sys:int32<< ,d 24)
                        (sys:int32-logior (sys:int32<< ,c 16)
                                          (sys:int32-logior (sys:int32<< ,b 8) ,a))))
    #-lw-int32
    `(the ub32 (logior (ash ,d 24) (ash ,c 16) (ash ,b 8) ,a))))

(deftype ub32-vector (length)
  #+lw-int32 (declare (ignore length))
  #+lw-int32 'sys:simple-int32-vector
  #-lw-int32 `(simple-array (unsigned-byte 32) (,length)))

(defmacro make-ub32-vector (length &rest args)
  #+lw-int32 `(sys:make-simple-int32-vector ,length ,@args)
  #-lw-int32 `(make-array ,length :element-type 'ub32 ,@args))

(defmacro ub32-aref (vector index)
  #+lw-int32
  `(sys:int32-aref ,vector ,index)
  #-lw-int32
  `(aref ,vector ,index))

;;; Section 3.4:  Auxilliary functions

(declaim (inline mod32+)
         (ftype (function (ub32 ub32) ub32) mod32+))
(defun mod32+ (a b)
  (declare (type ub32 a b)
           (optimize (speed 3) (safety 0) (space 0) (debug 0) #+lw-int32 (float 0)))
  #+lw-int32
  (lw-int32-no-overflow (sys:int32+ a b))
  #-lw-int32
  (ldb (byte 32 0) (+ a b)))

#+cmu
(define-compiler-macro mod32+ (a b)
  `(ext:truly-the ub32 (+ ,a ,b)))

;;; Dunno why we need this, but without it MOD32+ wasn't being
;;; inlined.  Oh well.  -- CSR, 2003-09-14
#+sbcl
(define-compiler-macro mod32+ (a b)
  `(ldb (byte 32 0) (+ ,a ,b)))

#+lw-int32
(declaim (inline int32>>logical)
         (ftype (function (sys:int32 (unsigned-byte 5)) sys:int32) int32>>logical))
#+lw-int32
(defun int32>>logical (a s)
  (declare (type ub32 a) (type (unsigned-byte 5) s)
           (optimize (speed 3) (safety 0) (space 0) (debug 0) #+lw-int32 (float 0)))
  (if (sys:int32-minusp a)
      (sys:int32-logandc2 (sys:int32>> a s) (sys:int32<< -1 (- 32 s)))
      (sys:int32>> a s)))

(declaim (inline rol32)
         (ftype (function (ub32 (unsigned-byte 5)) ub32) rol32))
(defun rol32 (a s)
  (declare (type ub32 a) (type (unsigned-byte 5) s)
           (optimize (speed 3) (safety 0) (space 0) (debug 0) #+lw-int32 (float 0)))
  #+cmu
  (kernel:32bit-logical-or #+little-endian (kernel:shift-towards-end a s)
                           #+big-endian (kernel:shift-towards-start a s)
                           (ash a (- s 32)))
  #+lw-int32
  (sys:int32-logior (lw-int32-no-overflow (sys:int32<< a s))
                    (int32>>logical a (- 32 s)))
  #-(or :cmu :lw-int32)
  (logior (ldb (byte 32 0) (ash a s)) (ash a (- s 32))))


#+cmu
(eval-when (:compile-toplevel :execute)
  (setq *features* *old-features*))

#+cmu
(eval-when (:compile-toplevel)
  (setq ext:*inline-expansion-limit* *old-expansion-limit*))

#+sbcl
(eval-when (:compile-toplevel :execute)
  (setq *features* *old-features*))

#+(and :lispworks (or (not :lispworks4) :lispworks4.4))
(eval-when (:compile-toplevel :execute)
  (setq *features* *old-features*))
