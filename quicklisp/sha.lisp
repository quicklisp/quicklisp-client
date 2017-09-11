;;;; sha.lisp

(in-package #:ql-sha)

;;; SHA-oriented arithmetic.

(deftype octet ()
  `(unsigned-byte 8))

(deftype ub32 ()
  `(unsigned-byte 32))

(deftype ub64 ()
  `(unsigned-byte 64))

(deftype octet-vector (&optional size)
  `(simple-array octet (,size)))

(defun octet-vector (&rest initial-contents)
  (make-array (length initial-contents)
              :element-type 'octet
              :initial-contents initial-contents))

(defun make-octet-vector (size)
  (make-array size :element-type 'octet))

(defun make-ub32-vector (size)
  (make-array size :element-type 'ub32))

(defun make-ub64-vector (size)
  (make-array size :element-type 'ub64))


;;; For every op, define 32 and 64 bit versions, binary versions, and
;;; n-ary versions that reduce by compiler macro to the binary
;;; version.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-mask (bits)
    (1- (ash 1 bits)))

  (defun expand-rest-to-binary (op args)
    (if (rest args)
        `(,op ,(first args)
              ,(expand-rest-to-binary op (rest args)))
        (first args)))

  (defun operation-expansion (lisp-fun type mask a b)
    `(logand ,mask (,lisp-fun (the ,type ,a)
                              (the ,type ,b))))

  (defun symconcat (symbol suffix)
    (intern (format nil "~A~A"
                    (symbol-name symbol)
                    suffix)
            *package*)))

(defmacro define-binary-op (name lisp-fun)
  (flet ((expand (name name/2 mask type)
           `((defun ,name/2 (a b)
               (declare (type ,type a b))
               (logand ,mask (,lisp-fun a b)))
             (define-compiler-macro ,name/2 (a b)
               (operation-expansion ',lisp-fun ',type ,mask a b))
             (defun ,name (&rest args)
               (reduce ',name/2 args))
             (define-compiler-macro ,name (&rest args)
               (expand-rest-to-binary ',name/2 args)))))
    (let* ((fun32 (symconcat name "32"))
           (fun64 (symconcat name "64"))
           (fun32/2 (symconcat fun32 "/2"))
           (fun64/2 (symconcat fun64 "/2"))
           (mask32 (make-mask 32))
           (mask64 (make-mask 64)))
      `(progn
         ,@(expand fun32 fun32/2 mask32 'ub32)
         ,@(expand fun64 fun64/2 mask64 'ub64)))))

(defmacro define-unary-op (name lisp-fun)
  (flet ((expand (name type mask)
           `((defun ,name (a)
               (declare (type ,type a))
               (logand ,mask (,lisp-fun a)))
             (define-compiler-macro ,name (a)
               `(logand ,',mask (,',lisp-fun (the ,',type ,a)))))))
    `(progn
       ,@(expand (symconcat name "32") 'ub32 (make-mask 32))
       ,@(expand (symconcat name "64") 'ub64 (make-mask 64)))))

(define-binary-op xor logxor)
(define-binary-op and logand)
(define-binary-op or logior)
(define-binary-op add +)
(define-unary-op not lognot)

(macrolet ((define-rotate (direction size)
             (let* ((type (ecase size (32 'ub32) (64 'ub64)))
                    (prefix (ecase direction (:left 'rol) (:right 'ror)))
                    (name (symconcat prefix (princ-to-string size)))
                    (mask (make-mask size)))
               (multiple-value-bind (shift1 shift2)
                   (ecase direction
                     (:right (values '(- count) `(- ,size count)))
                     (:left  (values 'count `(- count ,size))))
                 `(progn
                    (defun ,name (a count)
                      (declare (type ,type a)
                               (type (mod ,size) count))
                      (logand ,mask
                              (logior (ash a ,shift1)
                                      (logand ,mask (ash a ,shift2)))))
                    (define-compiler-macro ,name (&whole whole &rest args)
                      (declare (ignore args))
                      whole))))))
  (define-rotate :left 32)
  (define-rotate :left 64)
  (define-rotate :right 32)
  (define-rotate :right 64))

(macrolet ((define-shift (size)
             (let ((type (ecase size (32 'ub32) (64 'ub64)))
                   (name (symconcat 'shift (princ-to-string size)))
                   (mask (make-mask size)))
               `(progn
                  (defun ,name (a count)
                    (logand ,mask (ash (the ,type a) (- count))))
                  (define-compiler-macro ,name (a count)
                    `(logand ,',mask (ash (the ,',type ,a) (- ,count))))))))
  (define-shift 32)
  (define-shift 64))


;;; SHA

(defparameter *sha-buffer-size* 6400)

(defgeneric update-sha (sha source &key start end))
(defgeneric update-sha-from-file (sha file))
(defgeneric compress (sha buffer end))
(defgeneric block-octet-count (sha))
(defgeneric total-octet-count (sha))
(defgeneric make-trailer-octets (sha))
(defgeneric sha-word-size (sha))
(defgeneric hash-vector-octets (sha))
(defgeneric sha-trailer (sha))
(defgeneric finish-sha (sha))

(defun make-trailer (octet-count &key block-octet-count size-octets-count)
  "Returns a trailer suitable for use in SHA-1, SHA-256, or SHA-512
padding, depending on the parameters given in BLOCK-OCTET-COUNT and
SIZE-OCTETS-COUNT. OCTET-COUNT is the total number of octets hashed."
  ;; Need space for #(#x80 ...padding... <size-octets>)
  (let* ((room-needed (+ 1 size-octets-count))
         (room (- block-octet-count (rem octet-count block-octet-count)))
         (trailer-size room))
    (when (< room room-needed)
      (incf trailer-size block-octet-count))
    (let ((trailer (make-octet-vector trailer-size)))
      ;; Leading 1 bit
      (setf (aref trailer 0) #x80)
      (let ((bit-count (* octet-count 8)))
        (loop for i downfrom (1- trailer-size)
              for bit-offset from 0 by 8
              repeat size-octets-count
              do (setf (aref trailer i)
                       (ldb (byte 8 bit-offset) bit-count))))
      trailer)))


(defun decode-ub32-vector (octet-vector start count target-vector)
  "Convert the octets in OCTET-VECTOR as (unsigned-byte 32) values
into TARGET-VECTOR."
  (declare (optimize speed)
           (type octet-vector octet-vector)
           (type fixnum start count)
           (type (simple-array (unsigned-byte 32) (*)) target-vector))
  (flet ((decode (position)
           (logior (ash (aref octet-vector (+ position 0)) 24)
                   (ash (aref octet-vector (+ position 1)) 16)
                   (ash (aref octet-vector (+ position 2))  8)
                   (ash (aref octet-vector (+ position 3))  0))))
    (loop for i below count
          for j from start by 4
          do (setf (aref target-vector i) (decode j)))
    target-vector))

(defun decode-ub64-vector (octet-vector start count target-vector)
  "Convert the octets in OCTET-VECTOR as (unsigned-byte 64) values
into TARGET-VECTOR."
  (declare (optimize speed)
           (type octet-vector octet-vector)
           (type fixnum start count)
           (type (simple-array (unsigned-byte 64) (*)) target-vector))
  (flet ((decode (position)
           (logior (ash (aref octet-vector (+ position 0)) 56)
                   (ash (aref octet-vector (+ position 1)) 48)
                   (ash (aref octet-vector (+ position 2)) 40)
                   (ash (aref octet-vector (+ position 3)) 32)
                   (ash (aref octet-vector (+ position 4)) 24)
                   (ash (aref octet-vector (+ position 5)) 16)
                   (ash (aref octet-vector (+ position 6))  8)
                   (ash (aref octet-vector (+ position 7))  0))))
    (loop for i below count
          for j from start by 8
          do (setf (aref target-vector i) (decode j)))
    target-vector))


;;; Generic SHA structure

(defclass sha ()
  ((hash-vector
    :initarg :hash-vector
    :reader hash-vector)
   (work-vector
    :initarg :work-vector
    :reader work-vector)
   (buffer
    :initarg :buffer
    :reader buffer)
   (buffer-position
    :initarg :buffer-position
    :accessor buffer-position
    :initform 0)
   (total-octet-count
    :initarg :total-octet-count
    :initform 0
    :accessor total-octet-count)
   (block-octet-count
    :initarg :block-octet-count
    :reader block-octet-count)
   (word-size
    :initarg :word-size
    :reader sha-word-size)))

(defmethod sha-trailer (sha)
  (make-trailer (total-octet-count sha)
                :block-octet-count (block-octet-count sha)
                :size-octets-count (/ (sha-word-size sha) 4)))

(defun sha-result (sha)
  (hash-vector-octets sha))

(defmethod finish-sha (sha)
  (let ((trailer (sha-trailer sha)))
    (update-sha sha trailer)
    (compress sha (buffer sha) (buffer-position sha))
    (sha-result sha)))

(defmethod update-sha (sha octets &key start end)
  "Add OCTETS (delimited by START and END) to SHA."
  (unless start (setf start 0))
  (unless end (setf end (length octets)))
  ;; Copy as much of OCTETS to the SHA buffer as possible; if it
  ;; fills, COMPRESS it and copy more.
  (let* ((buffer (buffer sha))
         (pos (buffer-position sha))
         (buffer-size (length buffer))
         (capacity (- (length buffer) pos))
         (needed (- end start)))
    (incf (total-octet-count sha) needed)
    (loop
      (when (zerop capacity)
        (compress sha buffer buffer-size)
        (setf capacity buffer-size)
        (setf pos 0))
      (when (<= needed capacity)
        (replace buffer octets
                 :start1 pos
                 :start2 start
                 :end2 end)
        (incf (buffer-position sha) needed)
        (return))
      (replace buffer octets
               :start1 pos
               :end1 buffer-size
               :start2 start)
      (incf start capacity)
      (decf needed capacity)
      (setf capacity 0))))

(defmethod update-sha (sha (stream stream) &key start end)
  (declare (ignore start end))
  (let ((buffer (make-octet-vector *sha-buffer-size*)))
    (loop
      (let ((end (read-sequence buffer stream)))
        (when (zerop end)
          (return sha))
        (update-sha sha buffer :end end)))))

(defmethod update-sha-from-file (sha file)
  (with-open-file (stream file :element-type 'octet)
    (update-sha sha stream)))

(defmacro with-hash-vector (vars sha &body body)
  (let ((hash-vector (gensym "hash-vector")))
    `(let ((,hash-vector (hash-vector ,sha)))
       (let ,(loop for var in vars
                   for i from 0
                   collect (list var `(aref ,hash-vector ,i)))
         (progn
           ,@body)
         ,@(loop for var in vars
                 for i from 0
                 collect `(setf (aref ,hash-vector ,i) ,var))))))

(defun word-vector-octets (vector word-size)
  "Return an octet vector of the words of VECTOR, interpreting each
vector element as an unsigned-byte of size WORD-SIZE."
  (let ((result (make-octet-vector (* (length vector) (floor word-size 8)))))
    (dotimes (i (length result) result)
      (multiple-value-bind (word-index octet-index)
          (truncate (* i 8) word-size)
        (let ((ldb-position (- word-size octet-index 8)))
          (setf (aref result i)
                (ldb (byte 8 ldb-position) (aref vector word-index))))))))

(defmethod hash-vector-octets (sha)
  (word-vector-octets (hash-vector sha) (sha-word-size sha)))

;;; SHA-1

(defparameter *sha1-hash-vector*
  (vector #x67452301
          #xEFCDAB89
          #x98BADCFE
          #x10325476
          #xC3D2E1F0))

(defclass sha1 (sha)
  ()
  (:default-initargs
   :buffer (make-octet-vector *sha-buffer-size*)
   :hash-vector (replace (make-ub32-vector 5) *sha1-hash-vector*)
   :work-vector (make-ub32-vector 80)
   :block-octet-count 64
   :word-size 32))

(defmethod compress ((sha1 sha1) buffer end)
  (declare (optimize speed))
  (prog1 sha1
    (with-hash-vector (h0 h1 h2 h3 h4)
        sha1
      (let* ((f 0)
             (k 0)
             (w (work-vector sha1))
             (block-octet-count (block-octet-count sha1))
             (block-count (floor end block-octet-count)))
        (declare (type (simple-array ub32 (80)) w))
        (dotimes (ii block-count)
          (let ((a h0)
                (b h1)
                (c h2)
                (d h3)
                (e h4))
            (decode-ub32-vector buffer (* ii 64) 16 w)
            (loop for i from 16 to 79
                  do (setf (aref w i)
                           (rol32 (xor32 (aref w (- i 3))
                                         (aref w (- i 8))
                                         (aref w (- i 14))
                                         (aref w (- i 16)))
                                  1)))
            (dotimes (i 80)
              (cond ((<= 0 i 19)
                     (setf f (or32 (and32 b c)
                                   (and32 (not32 b) d)))
                     (setf k #x5A827999))
                    ((<= 20 i 39)
                     (setf f (xor32 b c d))
                     (setf k #x6ED9EBA1))
                    ((<= 40 i 59)
                     (setf f (or32 (and32 b c)
                                   (and32 b d)
                                   (and32 c d)))
                     (setf k #x8F1BBCDC))
                    ((<= 60 i 79)
                     (setf f (xor32 b c d))
                     (setf k #xCA62C1D6)))
              (let ((temp (add32 (rol32 a 5) f e k (aref w i))))
                (setf e d)
                (setf d c)
                (setf c (rol32 b 30))
                (setf b a)
                (setf a temp)))
            (setf h0 (add32 a h0))
            (setf h1 (add32 b h1))
            (setf h2 (add32 c h2))
            (setf h3 (add32 d h3))
            (setf h4 (add32 e h4))))
        (setf (buffer-position sha1) 0)))))


;;; SHA256

(defvar *sha256-round-constants*
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
    #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3
    #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
    #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
    #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
    #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
    #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
    #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208
    #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defvar *sha256-hash-vector*
  #(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
    #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

(defclass sha256 (sha)
  ((constant-vector
    :initarg :constant-vector
    :reader constant-vector))
  (:default-initargs
   :block-octet-count 64
   :buffer (make-octet-vector *sha-buffer-size*)
   :work-vector (make-ub32-vector 64)
   :hash-vector (replace (make-ub32-vector 8) *sha256-hash-vector*)
   :constant-vector (replace (make-ub32-vector 64)
                             *sha256-round-constants*)
   :word-size 32))

(defmethod compress ((sha sha256) buffer end)
  (declare (optimize speed))
  (prog1 sha
    (with-hash-vector (h0 h1 h2 h3 h4 h5 h6 h7)
        sha
      (let* ((w (work-vector sha))
             (k (constant-vector sha))
             (block-octet-count (block-octet-count sha))
             (block-count (floor end block-octet-count)))
        (declare (type (simple-array ub32 (*)) w k))
        (dotimes (ii block-count)
          (let ((a h0) (b h1) (c h2) (d h3)
                (e h4) (f h5) (g h6) (h h7))
            (declare (type ub32 a b c d e f g h))
            (decode-ub32-vector buffer (* ii 64) 16 w)
            (loop for i from 16 to 63
                  do
                  (let* ((w15 (aref w (- i 15)))
                         (w2 (aref w (- i 2)))
                         (s0 (xor32 (ror32 w15 7)
                                    (ror32 w15 18)
                                    (shift32 w15 3)))
                         (s1 (xor32 (ror32 w2 17)
                                    (ror32 w2 19)
                                    (shift32 w2 10))))
                    (setf (aref w i)
                          (add32 (aref w (- i 16))
                                 s0
                                 (aref w (- i 7))
                                 s1))))
            (dotimes (i 64)
              (let* ((S1 (xor32 (ror32 e 6) (ror32 e 11) (ror32 e 25)))
                     (ch (xor32 (and32 e f) (and32 (not32 e) g)))
                     (temp1 (add32 h S1 ch (aref k i) (aref w i)))
                     (S0 (xor32 (ror32 a 2) (ror32 a 13) (ror32 a 22)))
                     (maj (xor32 (and32 a b) (and32 a c) (and32 b c)))
                     (temp2 (add32 S0 maj)))
                (setf h g
                      g f
                      f e
                      e (add32 d temp1)
                      d c
                      c b
                      b a
                      a (add32 temp1 temp2))))
            (setf h0 (add32 a h0)
                  h1 (add32 b h1)
                  h2 (add32 c h2)
                  h3 (add32 d h3)
                  h4 (add32 e h4)
                  h5 (add32 f h5)
                  h6 (add32 g h6)
                  h7 (add32 h h7)))))
      (setf (buffer-position sha) 0))))


;;; SHA-512

(defvar *sha512-constant-vector*
  #(#x428a2f98d728ae22 #x7137449123ef65cd #xb5c0fbcfec4d3b2f #xe9b5dba58189dbbc
    #x3956c25bf348b538 #x59f111f1b605d019 #x923f82a4af194f9b #xab1c5ed5da6d8118
    #xd807aa98a3030242 #x12835b0145706fbe #x243185be4ee4b28c #x550c7dc3d5ffb4e2
    #x72be5d74f27b896f #x80deb1fe3b1696b1 #x9bdc06a725c71235 #xc19bf174cf692694
    #xe49b69c19ef14ad2 #xefbe4786384f25e3 #x0fc19dc68b8cd5b5 #x240ca1cc77ac9c65
    #x2de92c6f592b0275 #x4a7484aa6ea6e483 #x5cb0a9dcbd41fbd4 #x76f988da831153b5
    #x983e5152ee66dfab #xa831c66d2db43210 #xb00327c898fb213f #xbf597fc7beef0ee4
    #xc6e00bf33da88fc2 #xd5a79147930aa725 #x06ca6351e003826f #x142929670a0e6e70
    #x27b70a8546d22ffc #x2e1b21385c26c926 #x4d2c6dfc5ac42aed #x53380d139d95b3df
    #x650a73548baf63de #x766a0abb3c77b2a8 #x81c2c92e47edaee6 #x92722c851482353b
    #xa2bfe8a14cf10364 #xa81a664bbc423001 #xc24b8b70d0f89791 #xc76c51a30654be30
    #xd192e819d6ef5218 #xd69906245565a910 #xf40e35855771202a #x106aa07032bbd1b8
    #x19a4c116b8d2d0c8 #x1e376c085141ab53 #x2748774cdf8eeb99 #x34b0bcb5e19b48a8
    #x391c0cb3c5c95a63 #x4ed8aa4ae3418acb #x5b9cca4f7763e373 #x682e6ff3d6b2b8a3
    #x748f82ee5defb2fc #x78a5636f43172f60 #x84c87814a1f0ab72 #x8cc702081a6439ec
    #x90befffa23631e28 #xa4506cebde82bde9 #xbef9a3f7b2c67915 #xc67178f2e372532b
    #xca273eceea26619c #xd186b8c721c0c207 #xeada7dd6cde0eb1e #xf57d4f7fee6ed178
    #x06f067aa72176fba #x0a637dc5a2c898a6 #x113f9804bef90dae #x1b710b35131c471b
    #x28db77f523047d84 #x32caab7b40c72493 #x3c9ebe0a15c9bebc #x431d67c49c100d4c
    #x4cc5d4becb3e42b6 #x597f299cfc657e2a #x5fcb6fab3ad6faec #x6c44198c4a475817))

(defparameter *sha512-hash-vector*
  #(#x6a09e667f3bcc908 #xbb67ae8584caa73b #x3c6ef372fe94f82b
    #xa54ff53a5f1d36f1 #x510e527fade682d1 #x9b05688c2b3e6c1f
    #x1f83d9abfb41bd6b #x5be0cd19137e2179))

(defclass sha512 (sha)
  ((constant-vector
    :initarg :constant-vector
    :reader constant-vector))
  (:default-initargs
   :block-octet-count 128
   :buffer (make-octet-vector *sha-buffer-size*)
   :work-vector (make-ub64-vector 80)
   :hash-vector (replace (make-ub64-vector 8) *sha512-hash-vector*)
   :constant-vector (replace (make-ub64-vector 80)
                             *sha512-constant-vector*)
   :word-size 64))

(defmethod compress ((sha sha512) buffer end)
  (declare (optimize speed))
  (prog1 sha
    (with-hash-vector (h0 h1 h2 h3 h4 h5 h6 h7)
        sha
      (let* ((w (work-vector sha))
             (k (constant-vector sha))
             (block-octet-count (block-octet-count sha))
             (block-count (floor end block-octet-count)))
        (declare (type (simple-array ub64 (*)) w k))
        (dotimes (ii block-count)
          (let ((a h0) (b h1) (c h2) (d h3)
                (e h4) (f h5) (g h6) (h h7))
            (declare (type ub64 a b c d e f g h))
            (decode-ub64-vector buffer (* ii 128) 16 w)
            (loop for i from 16 to 79
                  do
                  (let* ((w15 (aref w (- i 15)))
                         (w2 (aref w (- i 2)))
                         (s0 (xor64 (ror64 w15 1)
                                    (ror64 w15 8)
                                    (shift64 w15 7)))
                         (s1 (xor64 (ror64 w2 19)
                                    (ror64 w2 61)
                                    (shift64 w2 6))))
                    (setf (aref w i)
                          (add64 (aref w (- i 16))
                                 s0
                                 (aref w (- i 7))
                                 s1))))
            (dotimes (i 80)
              (let* ((S1 (xor64 (ror64 e 14) (ror64 e 18) (ror64 e 41)))
                     (ch (xor64 (and64 e f) (and64 (not64 e) g)))
                     (temp1 (add64 h S1 ch (aref k i) (aref w i)))
                     (S0 (xor64 (ror64 a 28) (ror64 a 34) (ror64 a 39)))
                     (maj (xor64 (and64 a b) (and64 a c) (and64 b c)))
                     (temp2 (add64 S0 maj)))
                (setf h g
                      g f
                      f e
                      e (add64 d temp1)
                      d c
                      c b
                      b a
                      a (add64 temp1 temp2))))
            (setf h0 (add64 a h0)
                  h1 (add64 b h1)
                  h2 (add64 c h2)
                  h3 (add64 d h3)
                  h4 (add64 e h4)
                  h5 (add64 f h5)
                  h6 (add64 g h6)
                  h7 (add64 h h7)))))
      (setf (buffer-position sha) 0))))


;;; Misc. utility

(defun octet-vector-hex (octet-vector)
  (nstring-downcase
   (with-output-to-string (s)
     (map nil (lambda (o) (format s "~2,'0X" o)) octet-vector))))

(defun file-sha (sha-class file)
  (let ((sha (make-instance sha-class)))
    (with-open-file (stream file :element-type 'octet)
      (update-sha sha stream))
    (finish-sha sha)))

(defun file-sha-string (sha-class file)
  (octet-vector-hex (file-sha sha-class file)))
