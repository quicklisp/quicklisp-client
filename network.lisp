;;;
;;; Low-level networking implementations
;;;

(in-package #:ql-network)

(definterface host-address (host)
  (:implementation t
    host)
  (:implementation sbcl
    (ql-sbcl:host-ent-address (ql-sbcl:get-host-by-name host))))

(definterface open-connection (host port)
  (:implementation t
    (error "Sorry, quicklisp in implementation ~S is not supported yet."
           (lisp-implementation-type)))
  (:implementation allegro
    (ql-allegro:make-socket :remote-host host
                            :remote-port port))
  (:implementation abcl
    (let ((socket (qlb-abcl:make-socket host port)))
      (qlb-abcl:get-socket-stream socket :element-type '(unsigned-byte 8))))
  (:implementation ccl
    (ql-ccl:make-socket :remote-host host
                        :remote-port port))
  (:implementation clisp
    (ql-clisp:socket-connect port host :element-type '(unsigned-byte 8)))
  (:implementation cmucl
    (let ((fd (ql-cmucl:connect-to-inet-socket host port)))
      (ql-cmucl:make-fd-stream fd
                               :element-type '(unsigned-byte 8)
                               :binary-stream-p t
                               :input t
                               :output t)))
  (:implementation ecl
    (let* ((endpoint (ql-ecl:host-ent-address
                      (ql-ecl:get-host-by-name host)))
           (socket (make-instance 'ql-ecl:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (ql-ecl:socket-connect socket endpoint port)
      (ql-ecl:socket-make-stream socket
                                 :element-type '(unsigned-byte 8)
                                 :input t
                                 :output t
                                 :buffering :full)))
  (:implementation lispworks
    (ql-lispworks:open-tcp-stream host port
                                  :direction :io
                                  :read-timeout nil
                                  :element-type '(unsigned-byte 8)
                                  :timeout 5))
  (:implementation sbcl
    (let* ((endpoint (ql-sbcl:host-ent-address
                      (ql-sbcl:get-host-by-name host)))
           (socket (make-instance 'ql-sbcl:inet-socket
                                  :protocol :tcp
                                  :type :stream)))
      (ql-sbcl:socket-connect socket endpoint port)
      (ql-sbcl:socket-make-stream socket
                                  :element-type '(unsigned-byte 8)
                                  :input t
                                  :output t
                                  :buffering :full))))

(definterface read-octets (buffer connection)
  (:implementation t
    ;; ECL bug #3161786.
    (if (equal (stream-element-type connection)
               '(unsigned-byte 8))
      (read-sequence buffer connection)
      (let ((length (array-dimension buffer 0)))
        (loop for i from 0 to (1- length) do
          (setf (aref buffer i)
                (read-byte connection)))
        length)))
  (:implementation allegro
    (ql-allegro:read-vector buffer connection))
  (:implementation clisp
    (ql-clisp:read-byte-sequence buffer connection
                                  :no-hang nil
                                  :interactive t)))

(definterface write-octets (buffer connection)
  (:implementation t
    (write-sequence buffer connection)
    (finish-output connection)))

(definterface close-connection (connection)
  (:implementation t
    (ignore-errors (close connection))))

(definterface call-with-connection (host port fun)
  (:implementation t
    (let (connection)
      (unwind-protect
           (progn
             (setf connection (open-connection host port))
             (funcall fun connection))
        (when connection
          (close connection))))))

(defmacro with-connection ((connection host port) &body body)
  `(call-with-connection ,host ,port (lambda (,connection) ,@body)))
