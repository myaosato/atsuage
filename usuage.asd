#|
  This file is a part of usuage project.
|#

(defsystem "usuage"
  :version "0.1.0"
  :author "Satoaki Miyao"
  :license "MIT"
  :depends-on (:cl-fad :alexandria :local-time :cl-ppcre :rosa :cl-markdown)
  :components ((:module "src"
                :components
                ((:file "text")
                 (:file "files")
                 (:file "converter" :depends-on ("data"))
                 (:file "data" :depends-on ("files" "text"))
                 (:file "usuage" :depends-on ("files" "text" "converter")))))
  :description "Usuage is simple static site generator"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "usuage-test"))))
