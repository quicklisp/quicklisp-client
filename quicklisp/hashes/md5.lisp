;;;; hashes/md5.lisp

;;;; Code copied from https://github.com/pmai/md5/blob/master/md5.lisp
;;;; (see https://pmsf.eu/resources/lisp/MD5.html), and then the
;;;; following changes done:
;;;;
;;;; Removals:
;;;;
;;;; * md5sum-string so as to remove the flexi-streams dependency
;;;; * sb-rotate-byte:rotate-byte since it isn't in SBCL 1.3.14 on Debian 9
;;;; * testing code
;;;;
;;;; Changes:
;;;;
;;;; * package information at the top changed to fit quicklisp
;;;; * renamed f, g, h, i, *t*, fill-block-ub8, fill-block-char,
;;;;   fill-block, copy-to-buffer, +buffer-size+, and
;;;;   buffer-index to "md5-" prefixed versions
;;;;
;;;; Moved code:
;;;; * moved the defines related to features at the top, Section 2, part of
;;;;   Section 3.4, and the features related stuff at the bottom over to
;;;;   hash-base.lisp

;;;; This file implements The MD5 Message-Digest Algorithm, as defined in
;;;; RFC 1321 by R. Rivest, published April 1992.
;;;;
;;;; It was originally written by Pierre R. Mai, with copious input
;;;; from the cmucl-help mailing-list hosted at cons.org, in November
;;;; 2001 and has been placed into the public domain.  In the meantime
;;;; various fixes and improvements for other implementations as well
;;;; as maintenance have been provided by Christophe Rhodes, Alexey
;;;; Dejneka, Nathan Froyd, Andreas Fuchs, John Desoi, Dmitriy Ivanov,
;;;; and Kevin M. Rosenberg, and have been reintegrated into this
;;;; consolidated version by Pierre R. Mai.
;;;;
;;;; WARNING: The MD5 Message-Digest Algorithm has been compromised as
;;;; a cryptographically secure hash for some time, with known
;;;; theoretical and practical attacks.  Therefore use of this
;;;; implemenation is only recommended for legacy uses or uses which
;;;; do not require a cryptographically secure hash.  Use one of the
;;;; newer SHA-2 and SHA-3 secure hash standards, or whatever is
;;;; currently deemed cryptographically secure for all other uses.
;;;;
;;;; $Id$
;;;;
;;;; While the implementation should work on all conforming Common
;;;; Lisp implementations, it has originally been optimized for CMU
;;;; CL, where it achieved comparable performance to the standard
;;;; md5sum utility (within a factor of 1.5 or less on iA32 and
;;;; UltraSparc hardware).
;;;;
;;;; Currently, this implementation has also been optimized for SBCL
;;;; and LispWorks.
;;;;
;;;; Since the implementation makes heavy use of arithmetic on
;;;; (unsigned-byte 32) numbers, acceptable performance is likely only
;;;; on CL implementations that support unboxed arithmetic on such
;;;; numbers in some form.  This should include most 64bit CL
;;;; implementations.  For other CL implementations a 16bit
;;;; implementation of MD5 is probably more suitable.
;;;;
;;;; The code implements correct operation for files/sequences of
;;;; unbounded size as is, at the cost of having to do a single
;;;; generic integer addition for each call to update-md5-state.  If
;;;; you call update-md5-state frequently with little data, this can
;;;; pose a performance problem.  If you can live with a size
;;;; restriction of 512 MB, then you can enable fast fixnum arithmetic
;;;; by putting :md5-small-length onto *features* prior to compiling
;;;; this file.
;;;;
;;;; Testing code can be compiled by including :md5-testing on
;;;; *features* prior to compilation.  In that case evaluating
;;;; (md5::test-rfc1321) will run all the test-cases present in
;;;; Appendix A.5 of RFC 1321 and report on the results.
;;;; Evaluating (md5::test-other) will run further test-cases
;;;; gathered by the author to cover regressions, etc.
;;;;
;;;; This software is "as is", and has no warranty of any kind.  The
;;;; authors assume no responsibility for the consequences of any use
;;;; of this software.

(cl:in-package #:ql-hashes)

;;; Section 3.4:  Auxilliary functions

(declaim (inline md5-f md5-g md5-h md5-i)
         (ftype (function (ub32 ub32 ub32) ub32) f g h i))

(defun md5-f (x y z)
  (declare (type ub32 x y z)
           (optimize (speed 3) (safety 0) (space 0) (debug 0) #+lw-int32 (float 0)))
  #+cmu
  (kernel:32bit-logical-or (kernel:32bit-logical-and x y)
                           (kernel:32bit-logical-andc1 x z))
  #+lw-int32
  (sys:int32-logior (sys:int32-logand x y) (sys:int32-logandc1 x z))
  #-(or :cmu :lw-int32)
  (logior (logand x y) (logandc1 x z)))

(defun md5-g (x y z)
  (declare (type ub32 x y z)
           (optimize (speed 3) (safety 0) (space 0) (debug 0) #+lw-int32 (float 0)))
  #+cmu
  (kernel:32bit-logical-or (kernel:32bit-logical-and x z)
                           (kernel:32bit-logical-andc2 y z))
  #+lw-int32
  (sys:int32-logior (sys:int32-logand x z) (sys:int32-logandc2 y z))
  #-(or :cmu :lw-int32)
  (logior (logand x z) (logandc2 y z)))

(defun md5-h (x y z)
  (declare (type ub32 x y z)
           (optimize (speed 3) (safety 0) (space 0) (debug 0) #+lw-int32 (float 0)))
  #+cmu
  (kernel:32bit-logical-xor x (kernel:32bit-logical-xor y z))
  #+lw-int32
  (sys:int32-logxor x (sys:int32-logxor y z))
  #-(or :cmu :lw-int32)
  (logxor x y z))

(defun md5-i (x y z)
  (declare (type ub32 x y z)
           (optimize (speed 3) (safety 0) (space 0) (debug 0) #+lw-int32 (float 0)))
  #+cmu
  (kernel:32bit-logical-xor y (kernel:32bit-logical-orc2 x z))
  #+lw-int32
  (lw-int32-no-overflow (sys:int32-logxor y (sys:int32-logorc2 x z)))
  #-(or :cmu :lw-int32)
  (ldb (byte 32 0) (logxor y (logorc2 x z))))

;;; Section 3.4:  Table T

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *md5-t* (make-array 64 :element-type 'ub32
                                    :initial-contents
                                    (loop for i from 1 to 64
                                       collect
                                         (truncate
                                          (* 4294967296
                                             (abs (sin (float i 0.0d0)))))))))

;;; Section 3.4:  Helper Macro for single round definitions

#-lw-int32
(defmacro with-md5-round ((op block) &rest clauses)
  (loop for (a b c d k s i) in clauses
        collect
        `(setq ,a (mod32+ ,b (rol32 (mod32+ (mod32+ ,a (,op ,b ,c ,d))
                                            (mod32+ (ub32-aref ,block ,k)
                                                    ,(aref *md5-t* (1- i))))
                                    ,s)))
        into result
        finally
        (return `(progn ,@result))))

#+lw-int32
(defmacro with-md5-round ((op block) &rest clauses)
  (loop for (a b c d k s i) in clauses
        collect
        `(setq ,a (mod32+ ,b (rol32 (mod32+ (mod32+ ,a (,op ,b ,c ,d))
                                            (mod32+ (ub32-aref ,block ,k)
                                                    (sys:integer-to-int32 
                                                     ,(let ((t-val (aref *md5-t* (1- i))))
                                                        (dpb (ldb (byte 32 0) t-val)
                                                             (byte 32 0) 
                                                             (if (logbitp 31 t-val)
                                                                 -1
                                                                 0))))))
                                    ,s)))
        into result
        finally
        (return `(progn ,@result))))

;;; Section 3.3:  (Initial) MD5 Working Set

(deftype md5-regs ()
  "The working state of the MD5 algorithm, which contains the 4 32-bit
registers A, B, C and D."
  `(ub32-vector 4))

(defmacro md5-regs-a (regs)
  `(ub32-aref ,regs 0))

(defmacro md5-regs-b (regs)
  `(ub32-aref ,regs 1))

(defmacro md5-regs-c (regs)
  `(ub32-aref ,regs 2))

(defmacro md5-regs-d (regs)
  `(ub32-aref ,regs 3))

(defconstant +md5-magic-a+ (assemble-ub32 #x01 #x23 #x45 #x67)
  "Initial value of Register A of the MD5 working state.")
(defconstant +md5-magic-b+ (assemble-ub32 #x89 #xab #xcd #xef)
  "Initial value of Register B of the MD5 working state.")
(defconstant +md5-magic-c+ (assemble-ub32 #xfe #xdc #xba #x98)
  "Initial value of Register C of the MD5 working state.")
(defconstant +md5-magic-d+ (assemble-ub32 #x76 #x54 #x32 #x10)
  "Initial value of Register D of the MD5 working state.")

(declaim (inline initial-md5-regs))
(defun initial-md5-regs ()
  "Create the initial working state of an MD5 run."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0) #+lw-int32 (float 0)))
  (let ((regs (make-ub32-vector 4)))
    (declare (type md5-regs regs))
    (setf (md5-regs-a regs) +md5-magic-a+
          (md5-regs-b regs) +md5-magic-b+
          (md5-regs-c regs) +md5-magic-c+
          (md5-regs-d regs) +md5-magic-d+)
    regs))

;;; Section 3.4:  Operation on 16-Word Blocks

(deftype md5-block ()
  "The basic 16x32-bit word blocks that MD5 operates on."
  `(ub32-vector 16))

(defun update-md5-block (regs block)
  "This is the core part of the MD5 algorithm.  It takes a complete 16
word block of input, and updates the working state in A, B, C, and D
accordingly."
  (declare (type md5-regs regs)
           (type md5-block block)
           (optimize (speed 3) (safety 0) (space 0) (debug 0) #+lw-int32 (float 0)))
  (let ((A (md5-regs-a regs)) (B (md5-regs-b regs))
        (C (md5-regs-c regs)) (D (md5-regs-d regs)))
    (declare (type ub32 A B C D))
    ;; Round 1
    (with-md5-round (md5-f block)
      (A B C D  0  7  1)(D A B C  1 12  2)(C D A B  2 17  3)(B C D A  3 22  4)
      (A B C D  4  7  5)(D A B C  5 12  6)(C D A B  6 17  7)(B C D A  7 22  8)
      (A B C D  8  7  9)(D A B C  9 12 10)(C D A B 10 17 11)(B C D A 11 22 12)
      (A B C D 12  7 13)(D A B C 13 12 14)(C D A B 14 17 15)(B C D A 15 22 16))
    ;; Round 2
    (with-md5-round (md5-g block)
      (A B C D  1  5 17)(D A B C  6  9 18)(C D A B 11 14 19)(B C D A  0 20 20)
      (A B C D  5  5 21)(D A B C 10  9 22)(C D A B 15 14 23)(B C D A  4 20 24)
      (A B C D  9  5 25)(D A B C 14  9 26)(C D A B  3 14 27)(B C D A  8 20 28)
      (A B C D 13  5 29)(D A B C  2  9 30)(C D A B  7 14 31)(B C D A 12 20 32))
    ;; Round 3
    (with-md5-round (md5-h block)
      (A B C D  5  4 33)(D A B C  8 11 34)(C D A B 11 16 35)(B C D A 14 23 36)
      (A B C D  1  4 37)(D A B C  4 11 38)(C D A B  7 16 39)(B C D A 10 23 40)
      (A B C D 13  4 41)(D A B C  0 11 42)(C D A B  3 16 43)(B C D A  6 23 44)
      (A B C D  9  4 45)(D A B C 12 11 46)(C D A B 15 16 47)(B C D A  2 23 48))
    ;; Round 4
    (with-md5-round (md5-i block)
      (A B C D  0  6 49)(D A B C  7 10 50)(C D A B 14 15 51)(B C D A  5 21 52)
      (A B C D 12  6 53)(D A B C  3 10 54)(C D A B 10 15 55)(B C D A  1 21 56)
      (A B C D  8  6 57)(D A B C 15 10 58)(C D A B  6 15 59)(B C D A 13 21 60)
      (A B C D  4  6 61)(D A B C 11 10 62)(C D A B  2 15 63)(B C D A  9 21 64))
    ;; Update and return
    (setf (md5-regs-a regs) (mod32+ (md5-regs-a regs) A)
          (md5-regs-b regs) (mod32+ (md5-regs-b regs) B)
          (md5-regs-c regs) (mod32+ (md5-regs-c regs) C)
          (md5-regs-d regs) (mod32+ (md5-regs-d regs) D))
    regs))

;;; Section 3.4:  Converting 8bit-vectors into 16-Word Blocks

(declaim (inline md5-fill-block md5-fill-block-ub8 md5-fill-block-char))
(defun md5-fill-block-ub8 (block buffer offset)
  "Convert a complete 64 (unsigned-byte 8) input vector segment
starting from `offset' into the given 16 word MD5 block."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
           (type md5-block block)
           (type (simple-array (unsigned-byte 8) (*)) buffer)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)
                     #+lw-int32 (float 0) #+lw-int32 (hcl:fixnum-safety 0)))
  #+(and :cmu :little-endian)
  (kernel:bit-bash-copy
   buffer (+ (* vm:vector-data-offset vm:word-bits) (* offset vm:byte-bits))
   block (* vm:vector-data-offset vm:word-bits)
   (* 64 vm:byte-bits))
  #+(and :sbcl :little-endian)
  (sb-kernel:ub8-bash-copy buffer offset block 0 64)
  #-(or (and :sbcl :little-endian) (and :cmu :little-endian))
  (loop for i of-type (integer 0 16) from 0
        for j of-type (integer 0 #.most-positive-fixnum)
        from offset to (+ offset 63) by 4
        do
        (setf (ub32-aref block i)
              (assemble-ub32 (aref buffer j)
                             (aref buffer (+ j 1))
                             (aref buffer (+ j 2))
                             (aref buffer (+ j 3))))))

(defun md5-fill-block-char (block buffer offset)
  "DEPRECATED: Convert a complete 64 character input string segment
starting from `offset' into the given 16 word MD5 block."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
           (type md5-block block)
           (type simple-string buffer)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)
                     #+lw-int32 (float 0) #+lw-int32 (hcl:fixnum-safety 0)))
  #+(and :cmu :little-endian)
  (kernel:bit-bash-copy
   buffer (+ (* vm:vector-data-offset vm:word-bits) (* offset vm:byte-bits))
   block (* vm:vector-data-offset vm:word-bits)
   (* 64 vm:byte-bits))
  #+(and :sbcl :little-endian)
  (sb-kernel:ub8-bash-copy buffer offset block 0 64)
  #-(or (and :sbcl :little-endian) (and :cmu :little-endian))
  (loop for i of-type (integer 0 16) from 0
        for j of-type (integer 0 #.most-positive-fixnum)
        from offset to (+ offset 63) by 4
        do
        (setf (ub32-aref block i)
              (assemble-ub32 (char-code (schar buffer j))
                             (char-code (schar buffer (+ j 1)))
                             (char-code (schar buffer (+ j 2)))
                             (char-code (schar buffer (+ j 3)))))))

(defun md5-fill-block (block buffer offset)
  "Convert a complete 64 byte input vector segment into the given 16
word MD5 block.  This currently works on (unsigned-byte 8) and
character simple-arrays, via the functions `md5-fill-block-ub8' and
`md5-fill-block-char' respectively.  Note that it will not work correctly
on character simple-arrays if `char-code-limit' is greater than 256."
  (declare (type (integer 0 #.(- most-positive-fixnum 64)) offset)
           (type md5-block block)
           (type (simple-array * (*)) buffer)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)
                     #+lw-int32 (float 0) #+lw-int32 (hcl:fixnum-safety 0)))
  (etypecase buffer
    ((simple-array (unsigned-byte 8) (*))
     (md5-fill-block-ub8 block buffer offset))
    (simple-string
     (md5-fill-block-char block buffer offset))))

;;; Section 3.5:  Message Digest Output

(declaim (inline md5regs-digest))
(defun md5regs-digest (regs)
  "Create the final 16 byte message-digest from the MD5 working state
in `regs'.  Returns a (simple-array (unsigned-byte 8) (16))."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                     #+lw-int32 (float 0) #+lw-int32 (hcl:fixnum-safety 0))
           (type md5-regs regs))
  (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (16)) result))
    (macrolet ((frob (reg offset)
                 (let ((var (gensym)))
                   `(let ((,var #+lw-int32 (ldb (byte 32 0) (sys:int32-to-integer ,reg))
                                #-lw-int32 ,reg))
                      (declare (type (unsigned-byte 32) ,var))
                      (setf
                       (aref result ,offset) (ldb (byte 8 0) ,var)
                       (aref result ,(+ offset 1)) (ldb (byte 8 8) ,var)
                       (aref result ,(+ offset 2)) (ldb (byte 8 16) ,var)
                       (aref result ,(+ offset 3)) (ldb (byte 8 24) ,var))))))
      (frob (md5-regs-a regs) 0)
      (frob (md5-regs-b regs) 4)
      (frob (md5-regs-c regs) 8)
      (frob (md5-regs-d regs) 12))
    result))

;;; Mid-Level Drivers

(locally
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 1)
                     #+lw-int32 (float 0)))
(defstruct (md5-state
             (:constructor make-md5-state ())
             (:copier))
  (regs (initial-md5-regs) :type md5-regs :read-only t)
  (amount 0 :type
          #-md5-small-length (integer 0 *)
          #+md5-small-length (unsigned-byte 29))
  (block (make-ub32-vector 16) :read-only t :type md5-block)
  (buffer (make-array 64 :element-type '(unsigned-byte 8)) :read-only t
         :type (simple-array (unsigned-byte 8) (64)))
  (buffer-index 0 :type (integer 0 63))
  (finalized-p nil))
)

(declaim (inline md5-copy-to-buffer))
(defun md5-copy-to-buffer (from from-offset count buffer buffer-offset)
  "Copy a partial segment from input vector `from' starting at
`from-offset' and copying `count' elements into the 64 byte buffer
starting at `buffer-offset'."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                     #+lw-int32 (float 0) #+lw-int32 (hcl:fixnum-safety 0))
           (type (unsigned-byte 29) from-offset)
           (type (integer 0 63) count buffer-offset)
           (type (simple-array * (*)) from)
           (type (simple-array (unsigned-byte 8) (64)) buffer))
  #+cmu
  (kernel:bit-bash-copy
   from (+ (* vm:vector-data-offset vm:word-bits) (* from-offset vm:byte-bits))
   buffer (+ (* vm:vector-data-offset vm:word-bits)
             (* buffer-offset vm:byte-bits))
   (* count vm:byte-bits))
  #+sbcl
  (sb-kernel:ub8-bash-copy from from-offset buffer buffer-offset count)
  #-(or :cmu :sbcl)
  (etypecase from
    (simple-string
     (loop for buffer-index of-type (integer 0 64) from buffer-offset
           for from-index of-type fixnum from from-offset
           below (+ from-offset count)
           do
           (setf (aref buffer buffer-index)
                 (char-code (schar (the simple-string from) from-index)))))
    ((simple-array (unsigned-byte 8) (*))
     (loop for buffer-index of-type (integer 0 64) from buffer-offset
           for from-index of-type fixnum from from-offset
           below (+ from-offset count)
           do
           (setf (aref buffer buffer-index)
                 (aref (the (simple-array (unsigned-byte 8) (*)) from)
                       from-index))))))

(defun update-md5-state (state sequence &key (start 0) (end (length sequence)))
  "Update the given md5-state from `sequence', which is either a
simple-string or a simple-array with element-type (unsigned-byte 8),
bounded by `start' and `end', which must be numeric bounding-indices.
Note that usage on simple-strings is DEPRECATED, since this will not
work correctly if `char-code-limit' is more than 256.  String input
should be converted to (unsigned-byte 8) simple-arrays with
external-format conversion routines beforehand."
  (declare (type md5-state state)
           (type (simple-array * (*)) sequence)
           (type fixnum start end)
           (optimize (speed 3) (safety 1) (space 0) (debug 1)
                     #+lw-int32 (float 0) #+lw-int32 (hcl:fixnum-safety 0)))
  (locally 
      (declare (optimize (safety 0) (debug 0)))
    (let ((regs (md5-state-regs state))
          (block (md5-state-block state))
          (buffer (md5-state-buffer state))
          (buffer-index (md5-state-buffer-index state))
          (length (- end start)))
      (declare (type md5-regs regs) (type fixnum length)
               (type (integer 0 63) buffer-index)
               (type md5-block block)
               (type (simple-array (unsigned-byte 8) (64)) buffer))
      ;; Handle old rest
      (unless (zerop buffer-index)
        (let ((amount (min (- 64 buffer-index) length)))
          (declare (type (integer 0 63) amount))
          (md5-copy-to-buffer sequence start amount buffer buffer-index)
          (setq start (the fixnum (+ start amount)))
          (let ((new-index (+ buffer-index amount)))
            (when (= new-index 64)
              (md5-fill-block-ub8 block buffer 0)
              (update-md5-block regs block)
              (setq new-index 0))
            (when (>= start end)
              (setf (md5-state-buffer-index state) new-index
                    (md5-state-amount state)
                    #-md5-small-length (+ (md5-state-amount state) length)
                    #+md5-small-length (the (unsigned-byte 29)
                                         (+ (md5-state-amount state) length)))
              (return-from update-md5-state state)))))
      ;; Handle main-part and new-rest
      (etypecase sequence
        ((simple-array (unsigned-byte 8) (*))
           (locally
               (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
             (loop for offset of-type (unsigned-byte 29) from start below end by 64
                   until (< (- end offset) 64)
                   do
                (md5-fill-block-ub8 block sequence offset)
                (update-md5-block regs block)
                   finally
                (let ((amount (- end offset)))
                  (unless (zerop amount)
                    (md5-copy-to-buffer sequence offset amount buffer 0))
                  (setf (md5-state-buffer-index state) amount)))))
        (simple-string
           (locally
               (declare (type simple-string sequence))
             (loop for offset of-type (unsigned-byte 29) from start below end by 64
                   until (< (- end offset) 64)
                   do
                (md5-fill-block-char block sequence offset)
                (update-md5-block regs block)
                   finally
                (let ((amount (- end offset)))
                  (unless (zerop amount)
                    (md5-copy-to-buffer sequence offset amount buffer 0))
                  (setf (md5-state-buffer-index state) amount))))))
      (setf (md5-state-amount state)
            #-md5-small-length (+ (md5-state-amount state) length)
            #+md5-small-length (the (unsigned-byte 29)
                                 (+ (md5-state-amount state) length)))
      state)))

(defun finalize-md5-state (state)
  "If the given md5-state has not already been finalized, finalize it,
by processing any remaining input in its buffer, with suitable padding
and appended bit-length, as specified by the MD5 standard.

The resulting MD5 message-digest is returned as an array of sixteen
(unsigned-byte 8) values.  Calling `update-md5-state' after a call to
`finalize-md5-state' results in unspecified behaviour."
  (declare (type md5-state state)
           (optimize (speed 3) (safety 1) (space 0) (debug 1) #+lw-int32 (float 0)))
  (locally
      (declare (optimize (safety 0) (debug 0)))
    (or (md5-state-finalized-p state)
        (let ((regs (md5-state-regs state))
              (block (md5-state-block state))
              (buffer (md5-state-buffer state))
              (buffer-index (md5-state-buffer-index state))
              (total-length (* 8 (md5-state-amount state))))
          (declare (type md5-regs regs)
                   (type (integer 0 63) buffer-index)
                   (type md5-block block)
                   (type (simple-array (unsigned-byte 8) (*)) buffer))
          ;; Add mandatory bit 1 padding
          (setf (aref buffer buffer-index) #x80)
          ;; Fill with 0 bit padding
          (loop for index of-type (integer 0 64)
                  from (1+ buffer-index) below 64
                do (setf (aref buffer index) #x00))
          (md5-fill-block-ub8 block buffer 0)
          ;; Flush block first if length wouldn't fit
          (when (>= buffer-index 56)
            (update-md5-block regs block)
            ;; Create new fully 0 padded block
            (loop for index of-type (integer 0 16) from 0 below 16
                  do (setf (ub32-aref block index) #x00000000)))
          ;; Add 64bit message bit length
          (setf (ub32-aref block 14) (ldb (byte 32 0) total-length))
          #-md5-small-length
          (setf (ub32-aref block 15) (ldb (byte 32 32) total-length))
          ;; Flush last block
          (update-md5-block regs block)
          ;; Done, remember digest for later calls
          (setf (md5-state-finalized-p state)
                (md5regs-digest regs))))))

;;; High-Level Drivers

(defun md5sum-sequence (sequence &key (start 0) end)
  "Calculate the MD5 message-digest of data in `sequence', which should
be a 1d simple-array with element type (unsigned-byte 8).  On CMU CL
and SBCL non-simple and non-1d arrays with this element-type are also
supported.  Use with strings is DEPRECATED, since this will not work
correctly on implementations with `char-code-limit' > 256 and ignores
character-coding issues.  Convert to the required (unsigned-byte 8)
format through other means before-hand."
  (declare (optimize (speed 3) (safety 3) (space 0) (debug 1))
           (type vector sequence) (type fixnum start))
  (locally
      (declare (optimize (safety 1) (debug 0)))
    (let ((state (make-md5-state)))
      (declare (type md5-state state))
      #+cmu
      (let ((end (or end (length sequence))))
        (lisp::with-array-data ((data sequence) (real-start start) (real-end end))
          (declare (ignore real-end))
          (update-md5-state state data :start real-start
                            :end (+ real-start (- end start)))))
      #+sbcl
      (let ((end (or end (length sequence))))
        (sb-kernel:with-array-data ((data sequence)
                                    (real-start start)
                                    (real-end end)
                                    :check-fill-pointer t)
          (declare (ignore real-end))
          (update-md5-state state data :start real-start
                            :end (+ real-start (- end start)))))
      #-(or :cmu :sbcl)
      (let ((real-end (or end (length sequence))))
        (declare (type fixnum real-end))
        (update-md5-state state sequence :start start :end real-end))
      (finalize-md5-state state))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +md5-buffer-size+ (* 128 1024)
    "Size of internal buffer to use for `md5sum-stream' and `md5sum-file'
operations.  This should be a multiple of 64, the MD5 block size."))

(deftype md5-buffer-index () `(integer 0 ,+md5-buffer-size+))

(defun md5sum-stream (stream)
  "Calculate an MD5 message-digest of the contents of `stream'.  Its
element-type has to be (unsigned-byte 8). Use on character streams is
DEPRECATED, as this will not work correctly on implementations with
`char-code-limit' > 256 and ignores character coding issues."
  (declare (optimize (speed 3) (safety 3) (space 0) (debug 1)))
  (locally
      (declare (optimize (safety 1) (debug 0)))
    (let ((state (make-md5-state)))
      (declare (type md5-state state))
      (cond
        ((equal (stream-element-type stream) '(unsigned-byte 8))
         (let ((buffer (make-array +md5-buffer-size+
                                   :element-type '(unsigned-byte 8))))
           (declare (type (simple-array (unsigned-byte 8) (#.+md5-buffer-size+))
                          buffer))
           (loop for bytes of-type md5-buffer-index = (read-sequence buffer stream)
                 do (update-md5-state state buffer :end bytes)
                 until (< bytes +md5-buffer-size+)
                 finally
              (return (finalize-md5-state state)))))
        ((equal (stream-element-type stream) 'character)
         (let ((buffer (make-string +md5-buffer-size+)))
           (declare (type (simple-string #.+md5-buffer-size+) buffer))
           (loop for bytes of-type md5-buffer-index = (read-sequence buffer stream)
                 do (update-md5-state state buffer :end bytes)
                 until (< bytes +md5-buffer-size+)
                 finally
              (return (finalize-md5-state state)))))
        (t
         (error "Unsupported stream element-type ~S for stream ~S."
                (stream-element-type stream) stream))))))

(defun md5sum-file (pathname)
  "Calculate the MD5 message-digest of the file specified by `pathname'."
  (declare (optimize (speed 3) (safety 3) (space 0) (debug 1)))
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (md5sum-stream stream)))
