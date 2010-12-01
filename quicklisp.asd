;;;; quicklisp.asd

(asdf:defsystem #:quicklisp
  :serial t
  :version "2010113000"
  :components ((:file "package")
               (:file "utils")
               (:file "config")
               (:file "impl")
               (:file "impl-util")
               (:file "network")
               (:file "progress")
               (:file "http")
               (:file "deflate")
               (:file "minitar")
               (:file "dist")
               (:file "setup")
               (:file "client")
               (:file "client-update")
               (:file "dist-update")
               (:file "misc")))
