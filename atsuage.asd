#|
  This file is a part of atsuage project.
|#

(defsystem "atsuage"
  :version "0.2.0"
  :author "Satoaki Miyao"
  :license "MIT"
  :depends-on (:cl-fad :alexandria :local-time :cl-ppcre :rosa :cl-markdown :local-time)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "text")
                 (:file "files")
                 (:file "converter" :depends-on ("data"))
                 (:file "data" :depends-on ("files" "text" "utils"))
                 (:file "config" :depends-on ("files" "utils"))
                 (:file "theme" :depends-on ("utils"))
                 (:file "core" :depends-on ("files" "data" "converter" "config"))
                 (:file "atsuage" :depends-on ("core" "theme" "config")))))
  :description "Atsuage is simple static site generator"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown")))
