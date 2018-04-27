;;;; quicklisp-client-tests.asd

(asdf:defsystem #:quicklisp-client-tests
  :serial t
  :author "Zach Beane <zach@quicklisp.org>"
  :description "Tests for the Quicklisp client software."
  :license "MIT-style"
  :depends-on (#:fiasco)
  :components ((:file "package")
               (:file "sha")
               (:file "openpgp")
               (:file "cdb")))
