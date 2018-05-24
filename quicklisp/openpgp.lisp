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

(defclass rsa-signature-packet (packet)
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
   (public-key-algorithm
    :initarg :public-key-algorithm
    :accessor public-key-algorithm)
   (hash-algorithm
    :initarg :hash-algorithm
    :accessor hash-algorithm)
   (quick-check-value
    :initarg :quick-check-value
    :accessor quick-check-value)
   (signature-value
    :initarg :signature-value
    :accessor signature-value)))

(defmethod print-object ((packet rsa-signature-packet) stream)
  (print-unreadable-object (packet stream :type t :identity t)
    (format stream "~A key id ~S"
            (public-key-algorithm packet)
            (key-string (key-id packet)))))


(defclass rsa-public-key-packet (packet)
  ((fingerprint
    :initarg :fingerprint
    :accessor fingerprint)
   (key-id
    :initarg :key-id
    :accessor key-id)
   (hashed-data
    :initarg :hashed-data
    :accessor hashed-data)
   (creation-time
    :initarg :creation-time
    :accessor creation-time)
   (n
    :initarg :n
    :accessor n)
   (e
    :initarg :e
    :accessor e)))

(defclass rsa-public-subkey-packet (rsa-public-key-packet) ())

(defmethod print-object ((packet rsa-public-key-packet) stream)
  (print-unreadable-object (packet stream :type t :identity t)
    (format stream "key id ~S" (key-string (key-id packet)))))

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
  (24 . :subkey-binding-signature))


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
  (17 . :dsa))

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

(defun utf8-octets-to-string (octets)
  ;; FIXME: Handle real UTF-8
  (when (some (lambda (code) (logbitp 7 code)) octets)
    (error "Proper UTF-8 decoding is not implemented yet"))
  (map 'string 'code-char octets))

(defmethod specialize-packet-by-type ((packet-type (eql :user-id)) packet)
  (change-class packet
                'user-id-packet
                :user-id (utf8-octets-to-string (data packet))))


;;; Public key and subkey packets

(defmethod specialize-packet-by-type ((packet-type (eql :public-key)) packet)
  (let* ((pstream (pstream (data packet)))
         (version (read-u8 pstream)))
    (check-supported-value "version" 4 version)
    (let ((creation-time (read-u32 pstream))
          (public-key-algorithm (read-field-value 'public-key-algorithm
                                                  pstream)))
      (check-supported-value "public-key algorithm" :rsa public-key-algorithm)
      (let* ((n (read-mpi pstream))
             (e (read-mpi pstream))
             (fingerprint (compute-fingerprint (data packet)))
             (key-id (subseq fingerprint (- (length fingerprint) 8))))
        (change-class packet 'rsa-public-key-packet
                      :fingerprint fingerprint
                      :key-id key-id
                      :creation-time creation-time
                      :n n
                      :e e)))))

(defmethod specialize-packet-by-type ((packet-type (eql :public-subkey)) packet)
  (change-class (specialize-packet-by-type :public-key packet)
                'rsa-public-subkey-packet))


;;; Signature packet

(defmethod specialize-packet-by-type ((packet-type (eql :signature)) packet)
  (let* ((pstream (pstream (data packet)))
         (VERSION (read-u8 pstream)))
    (check-supported-value "version" 4 version)
    (let ((signature-type (read-field-value 'signature-type pstream))
          (public-key-algorithm (read-field-value 'public-key-algorithm
                                                  pstream))
          (hash-algorithm (read-field-value 'hash-algorithm pstream)))
      (check-supported-value "public-key algorithm"
                             :rsa
                             public-key-algorithm)
      (let* ((hashed-subpackets (read-signature-subpackets pstream))
             ;; Important to save the position immediately after
             ;; reading the hashed subpackets
             (end-of-hashed-data-pos (pos pstream))
             (unhashed-subpackets (read-signature-subpackets pstream))
             (subpackets (append hashed-subpackets unhashed-subpackets))
             (quick-check-value (read-n-octets 2 pstream))
             (rsa-signature-value (read-mpi pstream))
             (raw-creation-time (cdr (assoc :signature-creation-time subpackets)))
             (creation-time (and raw-creation-time
                                 (decode-u32 raw-creation-time))))
        (change-class packet 'rsa-signature-packet
                      :key-id (cdr (assoc :issuer subpackets))
                      :creation-time creation-time
                      :signature-type signature-type
                      :hash-algorithm hash-algorithm
                      :public-key-algorithm public-key-algorithm
                      :quick-check-value quick-check-value
                      :hashed-data (subseq (data packet)
                                           0 end-of-hashed-data-pos)
                      :signature-value rsa-signature-value)))))


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
    (check-type packet rsa-public-key-packet)
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
        (let* ((n (vector-integer result))
               (pk (expt-mod (signature-value signature)
                             (e public-key)
                             (n public-key))))
          (when (= n pk)
            :good-signature))))))
