;;;; Modified version of trivial-utf-8 by Marijn Haverbeke. See the
;;;; license information at the end of this file.

(in-package #:ql-utf-8)

;;; Minimal utf-8 decoding and encoding library.
;;;
;;; See http://common-lisp.net/project/trivial-utf-8/


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize* nil))

(deftype vector-index (&optional (size array-dimension-limit))
  `(integer 0 (,size)))

(deftype octet ()
  `(unsigned-byte 8))

(define-condition utf-8-decoding-error (simple-error)
  ((message :initarg :message)
   (byte :initarg :byte :initform nil))
  (:report (lambda (err stream)
             (format stream (slot-value err 'message)
                     (slot-value err 'byte)))))

(declaim (inline utf-8-group-size))
(defun utf-8-group-size (byte)
  "Determine the amount of bytes that are part of the character
starting with a given byte."
  (check-type byte octet)
  (cond ((zerop (logand byte #b10000000)) 1)
        ((= (logand byte #b11100000) #b11000000) 2)
        ((= (logand byte #b11110000) #b11100000) 3)
        ((= (logand byte #b11111000) #b11110000) 4)
        (t (error 'utf-8-decoding-error :byte byte
                  :message "Invalid byte at start of character: 0x~X"))))

(defun utf-8-string-length (bytes &key (start 0) (end (length bytes)))
  "Calculate the length of the string encoded by the given bytes."
  (check-type bytes (simple-array octet (*)))
  (check-type start vector-index)
  (check-type end vector-index)
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
  (check-type bytes (simple-array (unsigned-byte 8) (*)))
  (check-type start vector-index)
  (check-type group-size fixnum)
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

(defun decode-utf-8 (bytes-in &key (start 0) (end (length bytes-in)))
  "Convert a byte array containing utf-8 encoded characters into
the string it encodes."
  (check-type bytes-in vector)
  (check-type start vector-index)
  (check-type end vector-index)
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
        :finally (return buffer)))

;;; Copyright (c) Marijn Haverbeke
;;;
;;; This software is provided 'as-is', without any express or implied
;;; warranty. In no event will the authors be held liable for any
;;; damages arising from the use of this software.
;;;
;;; Permission is granted to anyone to use this software for any
;;; purpose, including commercial applications, and to alter it and
;;; redistribute it freely, subject to the following restrictions:
;;;
;;; 1. The origin of this software must not be misrepresented; you must
;;;    not claim that you wrote the original software. If you use this
;;;    software in a product, an acknowledgment in the product
;;;    documentation would be appreciated but is not required.
;;;
;;; 2. Altered source versions must be plainly marked as such, and must
;;;    not be misrepresented as being the original software.
;;;
;;; 3. This notice may not be removed or altered from any source
;;;    distribution.
