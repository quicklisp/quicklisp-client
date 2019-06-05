;;;; openpgp.lisp

(in-package #:ql-openpgp)

(defun first-n-octets (n vector)
  (let ((length (length vector)))
    (unless (<= n length)
      (error "Vector too short to take ~A elements" n))
    (subseq vector 0 n)))

(defun octet-vector-hex (octet-vector)
  (nstring-downcase
   (with-output-to-string (s)
     (map nil (lambda (o) (format s "~2,'0X" o)) octet-vector))))

(defun unix-universal-time (unix-time)
  (+ unix-time (encode-universal-time 0 0 0 1 1 1970 0)))


;;;; UTF-8 routines adapted lightly from trivial-utf-8

(declaim (inline utf-8-group-size))
(defun utf-8-group-size (byte)
  "Determine the amount of bytes that are part of the character
starting with a given byte."
  (declare (type fixnum byte))
  (cond ((zerop (logand byte #b10000000)) 1)
        ((= (logand byte #b11100000) #b11000000) 2)
        ((= (logand byte #b11110000) #b11100000) 3)
        ((= (logand byte #b11111000) #b11110000) 4)
        (t (error 'utf-8-decoding-error :byte byte
                  :message "Invalid byte at start of character: 0x~X"))))

(defun utf-8-string-length (bytes &key (start 0) (end (length bytes)))
  "Calculate the length of the string encoded by the given bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum start end))
  (loop :with i :of-type fixnum = start
        :with string-length = 0
        :while (< i end)
        :do (progn
              (incf (the fixnum string-length) 1)
              (incf i (utf-8-group-size (elt bytes i))))
        :finally (return string-length)))

(defun get-utf-8-character (bytes group-size &optional (start 0))
  "Given an array of bytes and the amount of bytes to use,
extract the character starting at the given start position."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum group-size start))
  (macrolet ((next-byte ()
               '(prog1 (elt bytes start)
                 (incf start)))
             (six-bits (byte)
               (let ((b (gensym)))
                 `(let ((,b ,byte))
                    (unless (= (logand ,b #b11000000) #b10000000)
                      (error 'utf-8-decoding-error :byte ,b
                             :message "Invalid byte 0x~X inside a character."))
                    (ldb (byte 6 0) ,b))))
             (test-overlong (byte min-size)
               (let ((b (gensym)))
                 `(let ((,b ,byte))
                    (unless (>= ,b ,min-size)
                      (error 'utf-8-decoding-error :byte ,b
                             :message "Overlong byte sequence found."))
                    ,b))))
    (case group-size
      (1 (next-byte))
      (2 (test-overlong (logior (ash (ldb (byte 5 0) (next-byte)) 6)
                                (six-bits (next-byte))) 128))
      (3 (test-overlong (logior (ash (ldb (byte 4 0) (next-byte)) 12)
                                (ash (six-bits (next-byte)) 6)
                                (six-bits (next-byte))) 2048))
      (4 (test-overlong (logior (ash (ldb (byte 3 0) (next-byte)) 18)
                                (ash (six-bits (next-byte)) 12)
                                (ash (six-bits (next-byte)) 6)
                                (six-bits (next-byte))) 65536)))))

(defun utf-8-octets-to-string (bytes-in)
  "Convert a byte array containing utf-8 encoded characters into
the string it encodes."
  (declare (type vector bytes-in))
  (let ((start 0)
        (end (length bytes-in)))
    (declare (type fixnum start end))
    (loop :with bytes = (coerce bytes-in '(simple-array (unsigned-byte 8) (*)))
          :with buffer = (make-string (utf-8-string-length bytes :start start :end end) :element-type 'character)
          :with array-position :of-type fixnum = start
          :with string-position :of-type fixnum = 0
          :while (< array-position end)
          :do (let* ((char (elt bytes array-position))
                     (current-group (utf-8-group-size char)))
                (when (> (+ current-group array-position) end)
                  (error 'utf-8-decoding-error
                         :message "Unfinished character at end of byte array."))
                (setf (char buffer string-position)
                      (code-char (get-utf-8-character bytes current-group
                                                      array-position)))
                (incf string-position 1)
                (incf array-position current-group))
          :finally (return buffer))))


;;;; r64.lisp

(defvar *radix64-alphabet*
  (concatenate 'string
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               "abcdefghijklmnopqrstuvwxyz"
               "0123456789"
               "+/"))
(defvar *whitespace-characters*
  '(#\Tab #\Newline #\Linefeed #\Page #\Return #\Space)
  "Whitespace standard characters as defined by http://l1sp.org/cl/2.1.4")

(defvar *whitespace-index* 255)

(defparameter *radix64-indexes*
  (let ((table (make-hash-table)))
    (setf (gethash #\= table) 0)
    (dolist (char *whitespace-characters*)
      (setf (gethash char table) *whitespace-index*))
    (loop for index from 0
          for char across *radix64-alphabet*
          do (setf (gethash char table) index))
    table))

(defstruct r64-decoder
  (state 0 :type (mod 4))
  (accumulator 0 :type (mod 256))
  (result (make-array 10 :element-type '(unsigned-byte 8)
                      :fill-pointer 0
                      :adjustable t)))

(defun update-decoder (decoder string)
  (declare (type r64-decoder decoder)
           (type string string)
           (optimize speed))
  (let ((state (r64-decoder-state decoder))
        (accumulator (r64-decoder-accumulator decoder))
        (result (r64-decoder-result decoder)))
    (dotimes (i (length string))
      (let* ((char (char string i))
             (index (gethash char *radix64-indexes* 100)))
        (declare (type (mod 256) index))
        (when (= index 100)
          (error "Invalid radix64 character ~S at ~A of ~S"
                 char i string))
        (when (eql index *whitespace-index*)
          (go skip))
        (ecase state
          (0
           (setf state 1)
           (setf accumulator (ash index 2)))
          (1
           (setf state 2)
           (unless (eql char #\=)
             (vector-push-extend (logior accumulator
                                         (ldb (byte 2 4) index))
                                 result))
           (setf accumulator (ash (ldb (byte 4 0) index) 4)))
          (2
           (setf state 3)
           (unless (eql char #\=)
             (vector-push-extend (logior accumulator (ldb (byte 4 2) index))
                                 result))
           (setf accumulator (ash (ldb (byte 2 0) index) 6)))
          (3
           (setf state 0)
           (unless (eql char #\=)
             (vector-push-extend (logior accumulator index)
                                 result))
           (setf accumulator 0)
           )))
      skip)
    (setf (r64-decoder-accumulator decoder) accumulator)
    (setf (r64-decoder-state decoder) state)
    decoder))

(defun r64-decode (string)
  "Decode a complete radix-64 message from STRING."
  (let ((decoder (make-r64-decoder)))
    (update-decoder decoder string)
    (r64-decoder-result decoder)))

;;;; ascii-armor.lisp

(define-condition ascii-armor-error (simple-error) ())

(defvar *supported-armor-header-lines*
  '("-----BEGIN PGP SIGNATURE-----"
    "-----BEGIN PGP PUBLIC KEY BLOCK-----"))

(defvar *supported-armor-tail-lines*
  '("-----END PGP SIGNATURE-----"
    "-----END PGP PUBLIC KEY BLOCK-----")
  "A list of supported tail lines. Must match up 1-to-1 with header
  lines.")

(defun starts-with (substring string)
  (and (<= (length substring) (length string))
       (string= substring string :end2 (length substring))))

(defun whitespacep (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun marker-equal (marker string)
  "Does STRING match MARKER? To match, MARKER must appear at the
  start, and have only whitespace following."
  (and (starts-with marker string)
       (not (position-if-not #'whitespacep string
                             :start (length marker)))))

(defun checksum-line-p (line)
  (and (<= (length line) 5)
       (char= (char line 0) #\=)))

(defun tail-line (header-line)
  (let ((index (position header-line *supported-armor-header-lines*
                         :test #'marker-equal)))
    (unless index
      (error "Unknown header line -- ~S" header-line))
    (elt *supported-armor-tail-lines* index)))

(defun skip-to-armor-header-line (stream)
  (loop for line = (read-line stream nil stream)
        when (eq line stream)
          do (error 'ascii-armor-error :format-control "EOF when looking for header")
        when (member line *supported-armor-header-lines* :test #'marker-equal)
        return line))

(defun ascii-armor-data (stream)
  "Return the ASCII-armored ASCII data from STREAM."
  (let* ((checksum-line nil)
         (header-line (skip-to-armor-header-line stream))
         (tail-line (tail-line header-line)))
    ;; Skip header lines, if present
    (loop for line = (read-line stream nil)
          if (null line)
            do (error 'ascii-armor-error
                      :format-control "Missing armor header lines")
          until (marker-equal "" line))
    ;; Read data
    (values
     (with-output-to-string (s)
       (loop for line = (read-line stream nil)
             if (null line)
               do (error 'ascii-armor-error
                         :format-control "End of file in armor stream")
             if (checksum-line-p line)
             ;; Skip leading #\=
             do (setf checksum-line (subseq line 1))
             until (marker-equal line tail-line)
             do (unless checksum-line (write-line line s))))
     checksum-line
     header-line)))

(defun ascii-armor-crc24 (octets)
  (let ((crc #xB704CE)
        (poly #x1864CFB))
    (map nil (lambda (octet)
               (setf crc (logand #xFFFFFF (logxor crc (ash octet 16))))
               (dotimes (i 8)
                 (setf crc (ash crc 1))
                 (when (logtest #x1000000 crc)
                   (setf crc (logxor crc poly)))))
         octets)
    (vector (ldb (byte 8 16) crc)
            (ldb (byte 8  8) crc)
            (ldb (byte 8  0) crc))))

(defun file-ascii-armor-data (file)
  (with-open-file (stream file)
    (multiple-value-bind (encoded-data encoded-checksum)
        (ascii-armor-data stream)
      (let* ((data (r64-decode encoded-data))
             (data-checksum (r64-decode encoded-checksum))
             (checksum (ascii-armor-crc24 data)))
        (unless (equalp checksum data-checksum)
          (error "Checksum mismatch -- ASCII armor data has ~S, locally computed ~S"
                 data-checksum
                 checksum))
        data))))


;;;; packet.lisp

(defun key-string (key-id)
  "Convert the octet vector KEY-ID to a hex string."
  (octet-vector-hex key-id))

(defclass packet ()
  ((packet-type
    :initarg :packet-type
    :accessor packet-type)
   (hashed-data
    :initarg :hashed-data
    :accessor hashed-data)
   (data
    :initarg :data
    :accessor data)))

(defgeneric version (packet)
  (:method (packet)
    (aref (data packet) 0)))

(defmethod print-object ((packet packet) stream)
  (print-unreadable-object (packet stream :type t :identity t)
    (format stream "~S, size ~D"
            (packet-type packet)
            (length (data packet)))))

(defmethod initialize-instance :after ((packet packet)
                                       &key data
                                         &allow-other-keys)
  (unless data
    (error "DATA is required"))
  (unless (slot-boundp packet 'hashed-data)
    (setf (hashed-data packet) data)))

(defgeneric specialize-packet-by-type (packet-type packet)
  (:method ((packet-type t) packet)
    packet))

(defgeneric specialize-packet (packet)
  (:documentation "Change (via CHANGE-CLASS) a plain packet into a
  specialized packet by examining its type and data.")
  (:method (packet)
    (specialize-packet-by-type (packet-type packet) packet)))


(defclass signature-packet (packet)
  ((key-id
    :initarg :key-id
    :accessor key-id)
   (signature-type
    :initarg :signature-type
    :accessor signature-type)
   (hashed-data
    :initarg :hashed-data
    :accessor hashed-data)
   (creation-time
    :initarg :creation-time
    :accessor creation-time)
   (expiration-time
    :initarg :expiration-time
    :accessor expiration-time)
   (public-key-algorithm
    :initarg :public-key-algorithm
    :accessor public-key-algorithm)
   (hash-algorithm
    :initarg :hash-algorithm
    :accessor hash-algorithm)
   (quick-check-value
    :initarg :quick-check-value
    :accessor quick-check-value)
   (subpackets
    :initarg :subpackets
    :reader subpackets)))

(defmethod print-object ((packet signature-packet) stream)
  (print-unreadable-object (packet stream :type t :identity t)
    (format stream "~A key id ~S"
            (public-key-algorithm packet)
            (key-string (key-id packet)))))

(defclass rsa-signature-packet (signature-packet)
  ((signature-value
    :initarg :signature-value
    :accessor signature-value)))

(defclass dsa-signature-packet (signature-packet)
  ((r
    :initarg :r
    :reader r)
   (s
    :initarg :s
    :reader s)))

(defgeneric expiredp (packet)
  (:method ((packet signature-packet))
    (let ((expired (expiration-time packet)))
      (and expired
           (< expired (get-universal-time))))))


(defclass public-key-packet (packet)
  ((fingerprint
    :initarg :fingerprint
    :accessor fingerprint)
   (key-id
    :initarg :key-id
    :accessor key-id)
   (creation-time
    :initarg :creation-time
    :accessor creation-time)))

(defmethod print-object ((packet public-key-packet) stream)
  (print-unreadable-object (packet stream :type t :identity t)
    (format stream "key id ~S" (key-string (key-id packet)))))

(defclass rsa-public-key-packet (public-key-packet)
  ((n
    :initarg :n
    :accessor n)
   (e
    :initarg :e
    :accessor e)
   (subkey-class-name
    :reader subkey-class-name
    :initform 'rsa-public-subkey-packet)))

(defclass rsa-public-subkey-packet (rsa-public-key-packet) ())

(defclass dsa-public-key-packet (public-key-packet)
  ((p
    :initarg :p
    :reader p)
   (q
    :initarg :q
    :reader q)
   (g
    :initarg :g
    :reader g)
   (y
    :initarg :y
    :reader y)
   (subkey-class-name
    :reader subkey-class-name
    :initform 'dsa-public-subkey-packet)))

(defclass dsa-public-subkey-packet (dsa-public-key-packet) ())

(defclass elgamal-public-key-packet (public-key-packet)
  ((p
    :initarg :p
    :reader p)
   (g
    :initarg :g
    :reader g)
   (y
    :initarg :y
    :reader y)
   (subkey-class-name
    :reader subkey-class-name
    :initform 'elgamal-public-subkey-packet)))

(defclass elgamal-public-subkey-packet (elgamal-public-key-packet) ())


(defvar *initial-fingerprint-vector*
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element #x99 ))

(defun compute-fingerprint (data)
  (let* ((sha1 (make-instance 'sha1))
         (length (length data))
         (length-vector (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref length-vector 0) (ldb (byte 8 8) length))
    (setf (aref length-vector 1) (ldb (byte 8 0) length))
    (update-sha sha1 *initial-fingerprint-vector*)
    (update-sha sha1 length-vector)
    (update-sha sha1 data)
    (finish-sha sha1)))

(defun compute-key-id (public-key)
  (subseq (fingerprint public-key) 12))

(defclass user-id-packet (packet)
  ((user-id
    :initarg :user-id
    :accessor user-id)))

(defmethod print-object ((packet user-id-packet) stream)
  (print-unreadable-object (packet stream :type t)
    (format stream "~S" (user-id packet))))


(defgeneric key-id-string (object)
  (:method (object)
    (key-string (key-id object))))


;;;; packet-parsing.lisp

(defclass packet-stream ()
  ((data
    :initarg :data
    :reader data)
   (data-length
    :reader data-length)
   (pos
    :initform 0
    :accessor pos)
   (eofp
    :initform nil
    :accessor eofp))
  (:documentation
   "A packet stream is a simple stream-like object for sequential
   access to an octet vector."))

(define-condition packet-stream-eof (error) ())

(defmethod initialize-instance :after ((packet-stream packet-stream)
                                       &key data &allow-other-keys)
  (unless data
    (error "DATA is required"))
  (setf (slot-value packet-stream 'data-length) (length data)))

(defun pstream (data)
  "Create a packet stream based on DATA."
  (make-instance 'packet-stream :data data))

(defun at-eof-p (packet-stream)
  (= (pos packet-stream) (data-length packet-stream)))

(defun check-eof (packet-stream)
  (when (or (eofp packet-stream)
            (setf (eofp packet-stream) (at-eof-p packet-stream)))
    (error 'packet-stream-eof)))


(defun read-u8 (packet-stream)
  (check-eof packet-stream)
  (prog1
      (aref (data packet-stream) (pos packet-stream))
    (incf (pos packet-stream))))

(defun read-n-octets (n pstream)
  (let ((vector (make-array n :element-type '(unsigned-byte 8))))
    (loop for i below n
          do (setf (aref vector i) (read-u8 pstream)))
    vector))

(defun read-u16 (packet-stream)
  (logior (ash (read-u8 packet-stream)  8)
          (ash (read-u8 packet-stream)  0)))

(defun read-u32 (packet-stream)
  (logior (ash (read-u8 packet-stream) 24)
          (ash (read-u8 packet-stream) 16)
          (ash (read-u8 packet-stream)  8)
          (ash (read-u8 packet-stream)  0)))

(defun read-mpi (packet-stream)
  (let* ((mpi-bits (read-u16 packet-stream))
         (octets (ceiling mpi-bits 8))
         (result 0))
    (dotimes (i octets result)
      (setf result (logior (ash result 8)
                           (read-u8 packet-stream))))))

(defun decode-u32 (vector)
  (logior (ash (aref vector 0) 24)
          (ash (aref vector 1) 16)
          (ash (aref vector 2)  8)
          (ash (aref vector 3)  0)))

(defun encode-u32 (u32)
  (make-array 4 :element-type '(unsigned-byte 8)
              :initial-contents (list (ldb (byte 8 24) u32)
                                      (ldb (byte 8 16) u32)
                                      (ldb (byte 8  8) u32)
                                      (ldb (byte 8  0) u32))))

(defun decode-u32-time (vector)
  (unix-universal-time (decode-u32 vector)))

(defun %reset (packet-stream)
  "Reset PACKET-STREAM so it can be read again from the beginning."
  (setf (pos packet-stream) 0)
  (setf (eofp packet-stream) nil)
  packet-stream)

(defun packet-type-value (packet-type)
  "Return a symbolic value for the integer PACKET-TYPE. Only supported
values are decoded; others signal an error. See RFC4880 section 4.3."
  (ecase packet-type
    (0 (error "0 is a reserved packet type and must not appear per RFC 4880"))
    (2 :signature)
    (6 :public-key)
    (13 :user-id)
    (14 :public-subkey)))


;;;
;;; Reading and decoding various packet fields from integers to
;;; symbolic constants.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-type-reader (type)
    (ecase type
      (u8 'read-u8)
      (u16 'read-u16)
      (u32 'read-u32))))

(defmacro define-field (name (&key type) &body values-alist)
  `(progn
     (setf (get ',name 'reader-function) ',(find-type-reader type))
     (setf (get ',name 'values-alist) ',values-alist)))

(defun missing-reader-function (&rest args)
  (declare (ignore args))
  (error "No reader function available"))

(defun read-field (field pstream)
  "Read the integer value of FIELD (a symbol previously defined with
DEFINE-FIELD) from PSTREAM."
  (funcall (get field 'reader-function 'missing-reader-function) pstream))

(defun read-field-value (field pstream &key (default nil defaultp))
  "Read and FIELD from PSTREAM and look up and return its symbolic
value."
  (let* ((raw-value (read-field field pstream))
         (translation (assoc raw-value (get field 'values-alist))))
    (when (and (not translation)
               (not defaultp))
      (error "Unsupported value ~A for field ~A"
             raw-value
             field))
    (if translation
        (cdr translation)
        default)))

(define-field signature-type (:type u8)
  ;; RFC 4880 section 5.2.1
  (0 . :binary-document)
  (16 . :generic-certification)
  (17 . :persona-certification)
  (18 . :casual-certification)
  (19 . :positive-user-id-certification)
  (24 . :subkey-binding-signature)
  (48 . :certification-revokation-signature))


(define-field subpacket-type (:type u8)
  ;; RFC 4880 section 5.2.3.1
  (2 . :signature-creation-time)
  (3 . :signature-expiration-time)
  (9 . :key-expiration-time)
  (11 . :preferred-symmetric-algorithms)
  (16 . :issuer)
  (21 . :preferred-hash-algorithms)
  (22 . :preferred-compression-algorithms)
  (23 . :key-server-preferences)
  (27 . :key-flags)
  (28 . :signer-user-id)
  (30 . :features))

(define-field public-key-algorithm (:type u8)
  ;; RFC 4880 section 9.1
  (1 . :rsa)
  (3 . :rsa-sign-only)
  (16 . :elgamal)
  (17 . :dsa)
  (19 . :ecdsa))

(define-field hash-algorithm (:type u8)
  ;; RFC 4880 section 9.4
  (1 . :md5)
  (2 . :sha-1)
  (8 . :sha-256)
  (9 . :sha-384)
  (10 . :sha-512)
  (11 . :sha-224))


(defun check-supported-value (description supported actual)
  "Signal an error unless SUPPORTED is EQL to ACTUAL"
  (unless (eql supported actual)
    (error "Value ~S for OpenPGP ~A not supported -- only ~A"
           actual description supported)))

(defun read-subpacket-length (pstream)
  "Read an encoded length value, which may be 1, 2, or 5 octets in
size, from PSTREAM. See RFC4880 5.2.3.1 for details."
  (let ((b1 (read-u8 pstream)))
    (cond ((< b1 192)
           b1)
          ((<= 192 b1 254)
           (let ((b2 (read-u8 pstream)))
             (logior (ash (- b1 192) 8)
                     b2
                     192)))
          ((= b1 255)
           (read-u32 pstream)))))

(defun read-signature-subpacket (pstream)
  "Read a single signature subpacket from PSTREAM. Returns the packet
  type and data as multiple values."
  (let* ((length (read-subpacket-length pstream))
         (type (read-field-value 'subpacket-type pstream
                                 :default :unrecognized))
         (data (read-n-octets (1- length) pstream)))
    (values type data)))

(defun read-signature-subpackets (pstream)
  "Read a list of subpackets from PSTREAM."
  (let* ((subpackets-total-length (read-u16 pstream))
         (end (+ (pos pstream) subpackets-total-length))
         (result '()))
    (loop
      (when (<= end (pos pstream))
        (return (nreverse result)))
      (multiple-value-bind (type data)
          (read-signature-subpacket pstream)
        (push (cons type data) result)))))

;;; Generic packet reading

(defun read-packet (pstream)
  "Read a packet from PSTREAM. Signals PACKET-STREAM-EOF if there is
no more data in PSTREAM. Format of binary packet data header is
specified in RFC 4880 section 4.2."
  (let ((tag (read-u8 pstream)))
    (unless (logbitp 7 tag)
      (error "Invalid packet tag -- bit 7 is zero -- ~A" tag))
    (when (logbitp 6 tag)
      (error "New packet format is not supported"))
    (let* ((packet-tag (ldb (byte 4 2) tag))
           (length-type (ldb (byte 2 0) tag))
           (length-size (expt 2 length-type)))
      (when (= length-type 3)
        (error "Indefinite length types not supported"))
      (let* ((length (ecase length-size
                       (1 (read-u8 pstream))
                       (2 (read-u16 pstream))
                       (4 (read-u16 pstream))))
             (data (read-n-octets length pstream)))
        (specialize-packet
         (make-instance 'packet
                        :data data
                        :packet-type (packet-type-value packet-tag)))))))

(defun read-packets (pstream)
  "Return a list of packets from PSTREAM."
  (loop for packet = (handler-case (read-packet pstream)
                       (packet-stream-eof () nil))
        while packet
        collect packet))

;;; User-id packets

(defmethod specialize-packet-by-type ((packet-type (eql :user-id)) packet)
  (change-class packet
                'user-id-packet
                :user-id (utf8-octets-to-string (data packet))))


;;; Public key and subkey packets

(defun specialize-key (packet class pstream &rest initargs)
  (let ((args (loop for initarg in initargs
                    collect initarg
                    collect (read-mpi pstream))))
    (apply #'change-class packet class args)))

(defmethod specialize-packet-by-type ((packet-type (eql :public-key)) packet)
  (let* ((pstream (pstream (data packet)))
         (version (read-u8 pstream)))
    (check-supported-value "version" 4 version)
    (let* ((creation-time (read-u32 pstream))
           (public-key-algorithm (read-field-value 'public-key-algorithm
                                                   pstream))
           (fingerprint (compute-fingerprint (data packet)))
           (key-id (subseq fingerprint (- (length fingerprint) 8))))
      (change-class packet 'public-key-packet
                    :creation-time (unix-universal-time creation-time)
                    :fingerprint fingerprint
                    :key-id key-id)
      (ecase public-key-algorithm
        (:rsa
         (specialize-key packet 'rsa-public-key-packet pstream
                         ':n ':e))
        (:dsa
         (specialize-key packet 'dsa-public-key-packet pstream
                         ':p ':q ':g ':y))
        (:elgamal
         (specialize-key packet 'elgamal-public-key-packet pstream
                         ':p ':g ':y))))))

(defmethod specialize-packet-by-type ((packet-type (eql :public-subkey)) packet)
  (let ((specialized (specialize-packet-by-type :public-key packet)))
    (change-class specialized
                  (subkey-class-name specialized))))


;;; Signature packet

(defmethod specialize-packet-by-type ((packet-type (eql :signature)) packet)
  (let* ((pstream (pstream (data packet)))
         (VERSION (read-u8 pstream)))
    (check-supported-value "version" 4 version)
    (let ((signature-type (read-field-value 'signature-type pstream))
          (public-key-algorithm (read-field-value 'public-key-algorithm
                                                  pstream))
          (hash-algorithm (read-field-value 'hash-algorithm pstream)))
      (let* ((hashed-subpackets (read-signature-subpackets pstream))
             ;; Important to save the position immediately after
             ;; reading the hashed subpackets
             (end-of-hashed-data-pos (pos pstream))
             (unhashed-subpackets (read-signature-subpackets pstream))
             (subpackets (append hashed-subpackets unhashed-subpackets))
             (quick-check-value (read-n-octets 2 pstream))
             (raw-creation-time (cdr (assoc :signature-creation-time subpackets)))
             (raw-expiration-time (cdr (assoc :key-expiration-time subpackets)))
             (creation-time (decode-u32-time raw-creation-time))
             (expiration-time (and raw-expiration-time
                                   (+ creation-time
                                      (decode-u32 raw-expiration-time)))))
        (change-class packet 'signature-packet
                      :key-id (cdr (assoc :issuer subpackets))
                      :subpackets subpackets
                      :creation-time creation-time
                      :expiration-time expiration-time
                      :signature-type signature-type
                      :hash-algorithm hash-algorithm
                      :public-key-algorithm public-key-algorithm
                      :quick-check-value quick-check-value
                      :hashed-data (subseq (data packet)
                                           0 end-of-hashed-data-pos)
                      :subpackets subpackets)
        (ecase public-key-algorithm
          (:rsa
           (let ((rsa-signature-value (read-mpi pstream)))
             (change-class packet 'rsa-signature-packet
                           :signature-value rsa-signature-value)))
          (:dsa
           (let ((r (read-mpi pstream))
                 (s (read-mpi pstream)))
             (change-class packet 'dsa-signature-packet
                           :r r
                           :s s))))))))


;;; Misc

(defun load-packets-from-file (file)
  (let* ((data (file-ascii-armor-data file))
         (pstream (pstream data)))
    (read-packets pstream)))

(defun load-packet-from-file (file)
  (let* ((data (file-ascii-armor-data file))
         (pstream (pstream data)))
    (read-packet pstream)))


;;;; signature.lisp

(defun expt-mod (n exponent modulus)
  (loop with result = 1
        for i from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
        (setf result (mod (* result sqr) modulus))
        finally (return result)))

(defun vector-integer (vector)
  "Convert the octet vector VECTOR to an integer."
  (let ((result 0))
    (dotimes (i (length vector) result)
      (setf result (logior (ash result 8) (aref vector i))))))


(defun load-signature (file)
  (let* ((packet (load-packet-from-file file)))
    (check-type packet rsa-signature-packet)
    packet))

(defun load-public-key (file)
  (let* ((packet (load-packet-from-file file)))
    (check-type packet public-key-packet)
    packet))

(defun verify-signature (file signature public-key)
  (unless (equalp (key-id public-key)
                  (key-id signature))
    (error "Signature and public key do not match"))
  (check-supported-value "hash algorithm" :sha-512 (hash-algorithm signature))
  (check-supported-value "public-key algorithm"
                         :rsa
                         (public-key-algorithm signature))
  (check-supported-value "signature type"
                         :binary-document
                         (signature-type signature))
  (let ((sha512 (make-instance 'sha512))
        (trailer (make-array 2 :element-type '(unsigned-byte 8)
                               :initial-contents (list (version signature) #xFF)))
        (size-vector (encode-u32 (length (hashed-data signature))))
        (quick-check-expected (quick-check-value signature)))
    (update-sha-from-file sha512 file)
    (update-sha sha512 (hashed-data signature))
    (update-sha sha512 trailer)
    (update-sha sha512 size-vector)
    (let* ((result (finish-sha sha512))
           (quick-check-actual (first-n-octets 2 result)))
      (when (equalp quick-check-actual quick-check-expected)
        ;; The RESULT vector encodes a 512-bit integer, while the pk
        ;; integer can be many more bits than that. Only compare N to
        ;; the low 512 bits of pk for signature checking.
        (let* ((n (vector-integer result))
               (pk (ldb (byte 512 0)
                        (expt-mod (signature-value signature)
                                  (e public-key)
                                  (n public-key)))))
          (when (= n pk)
            :good-signature))))))

(defun verify-certification-signature (data signature public-key)
  (unless (equalp (key-id public-key)
                  (key-id signature))
    (error "Signature and public key do not match"))
  (check-supported-value "hash algorithm" :sha-512 (hash-algorithm signature))
  (check-supported-value "public-key algorithm"
                         :rsa
                         (public-key-algorithm signature))
  (check-supported-value "signature type"
                         :positive-user-id-certification
                         (signature-type signature))
  (let ((sha512 (make-instance 'sha512))
        (minibuffer (make-array 1 :element-type '(unsigned-byte 8)))
        (trailer (make-array 2 :element-type '(unsigned-byte 8)
                               :initial-contents (list (version signature) #xFF)))
        (size-vector (encode-u32 (length (hashed-data signature))))
        (quick-check-expected (quick-check-value signature)))
    (flet ((hash-constant (c)
             (fill minibuffer c)
             (update-sha sha512 minibuffer)))
      (hash-constant #x99)
      (update-sha sha512 (subseq (encode-u32 (length (data public-key))) 2))
      (update-sha sha512 (data public-key))
      (hash-constant #xB4)
      (update-sha sha512 (encode-u32 (length data)))
      (update-sha sha512 data)
      (update-sha sha512 (hashed-data signature))
      (update-sha sha512 trailer)
      (update-sha sha512 size-vector))
    (let* ((result (finish-sha sha512))
           (quick-check-actual (first-n-octets 2 result)))
      (when (equalp quick-check-actual quick-check-expected)
        ;; The RESULT vector encodes a 512-bit integer, while the pk
        ;; integer can be many more bits than that. Only compare N to
        ;; the low 512 bits of pk for signature checking.
        (let* ((n (vector-integer result))
               (pk (ldb (byte 512 0)
                        (expt-mod (signature-value signature)
                                  (e public-key)
                                  (n public-key)))))
          (when (= n pk)
            :good-signature))))))
