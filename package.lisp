;;;; package.lisp

(defpackage #:ql-util
  (:use #:cl)
  (:export #:write-line-to-file
           #:press-enter-to-continue
           #:replace-file
           #:delete-file-if-exists
           #:ensure-file-exists
           #:split-spaces
           #:first-line))

(defpackage #:ql-setup
  (:use #:cl)
  (:export #:qmerge
           #:qenough
           #:*quicklisp-home*))

(defpackage #:ql-config
  (:use #:cl #:ql-util #:ql-setup)
  (:export #:config-value))

(defpackage #:ql-impl
  (:use #:cl)
  (:export #:*implementation*)
  (:export #:definterface
           #:defimplementation
           #:defimplementation-case)
  (:export #:lisp
           #:abcl
           #:allegro
           #:ccl
           #:clisp
           #:cmucl
           #:cormanlisp
           #:ecl
           #:gcl
           #:lispworks
           #:scl
           #:sbcl))

(defpackage #:ql-impl-util
  (:use #:cl #:ql-impl)
  (:export #:call-with-quiet-compilation
           #:add-to-init-file
           #:rename-directory
           #:delete-directory
           #:directory-entries
           #:delete-directory-tree))

(defpackage #:ql-network
  (:use #:cl #:ql-impl)
  (:export #:open-connection
           #:write-octets
           #:read-octets
           #:close-connection
           #:with-connection))

(defpackage #:ql-progress
  (:use #:cl)
  (:export #:make-progress-bar
           #:start-display
           #:update-progress
           #:finish-display))

(defpackage #:ql-http
  (:use #:cl #:ql-network #:ql-progress #:ql-config)
  (:export #:*proxy-url*
           #:fetch
           #:hostname
           #:port
           #:path
           #:url
           #:*maximum-redirects*
           #:*default-url-defaults*))

(defpackage #:ql-minitar
  (:use #:cl)
  (:export #:tarball-contents
           #:unpack-tarball))

(defpackage #:ql-gunzipper
  (:use #:cl)
  (:export #:gunzip))

(defpackage #:ql-release-index
  (:use #:cl)
  (:export #:load-quicklisp-index
           #:find-system
           #:find-release
           #:find-systems-named
           #:find-releases-named
           #:depends-on
           #:project-name
           #:digest
           #:prefix
           #:release
           #:system-paths
           #:required-systems
           #:download-url
           #:*quicklisp-index*))

(defpackage #:ql-dist
  (:use #:cl
        #:ql-util
        #:ql-http
        #:ql-setup
        #:ql-gunzipper
        #:ql-minitar)
  (:import-from #:ql-impl-util
                #:delete-directory-tree
                #:directory-entries)
  ;; Install/enable protocol
  (:export #:installedp
           #:install
           #:uninstall
           #:ensure-installed
           #:enabledp
           #:enable
           #:disable)
  ;; Preference protocol
  (:export #:preference
           #:preference-file
           #:preference-parent
           #:forget-preference)
  ;; Generic
  (:export #:all-dists
           #:enabled-dists
           #:find-dist
           #:find-system
           #:find-release
           #:dist
           #:system
           #:release
           #:base-directory
           #:relative-to
           #:metadata-name
           #:install-metadata-file
           #:short-description
           #:provided-releases
           #:provided-systems
           #:installed-releases
           #:installed-systems
           #:name)
  ;; Dists
  (:export #:dist
           #:dist-merge
           #:find-system-in-dist
           #:find-release-in-dist
           #:system-index-url
           #:release-index-url
           #:version
           #:subscription-url
           #:new-version-available-p
           #:dist-difference
           #:fetch-dist
           #:initialize-release-index
           #:initialize-system-index
           #:with-consistent-dists)
  ;; Dist updates
  (:export #:available-update
           #:update-release-differences
           #:show-update-report
           #:update-in-place)
  ;; Releases
  (:export #:release
           #:project-name
           #:system-files
           #:archive-url-suffix
           #:archive-size
           #:ensure-archive-file
           #:archive-content-sha1
           #:archive-file-md5
           #:prefix
           #:local-archive-file
           #:ensure-local-archive-file
           #:local-archive-file-valid-p)
  ;; Systems
  (:export #:dist
           #:release
           #:preference
           #:system-file-name
           #:required-systems)
  ;; Misc
  (:export #:standard-dist-enumeration-function
           #:*dist-enumeration-functions*
           #:find-asdf-system-file
           #:system-apropos
           #:dependency-tree
           #:clean))

(defpackage #:quicklisp-client
  (:nicknames #:quicklisp #:ql)
  (:export #:quickload
           #:*quickload-prompt*
           #:*quickload-verbose*
           #:*quickload-explain*
           #:uninstall
           #:uninstall-dist
           #:qmerge
           #:*quicklisp-home*
           #:*initial-dist-url*
           #:*proxy-url*
           #:config-value
           #:setup
           #:provided-systems
           #:system-apropos
           #:system-list
           #:update-client
           #:update-dist
           #:update-all-dists
           #:add-to-init-file
           #:use-only-quicklisp-systems
           #:help)
  (:shadow #:uninstall)
  (:use #:cl
        #:ql-util
        #:ql-impl-util
        #:ql-dist
        #:ql-http
        #:ql-setup
        #:ql-config
        #:ql-minitar
        #:ql-gunzipper))

(in-package #:quicklisp-client)

