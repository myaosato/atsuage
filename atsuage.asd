#|
  This file is a part of atsuage project.
|#

(defsystem "atsuage"
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
                 (:file "core" :depends-on ("files" "data" "converter"))
                 (:file "atsuage" :depends-on ("core")))))
  :description "Atsuage is simple static site generator"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown")))
