(in-package :cl-user)
(defpackage usuage.text
  (:use :cl)
  (:import-from :rosa
                :peruse-as-plist
                :indite)
  (:export :get-data-from-text
           :set-data-to-text))
(in-package :usuage.text)

(defun get-data-from-text (pathname &optional (upcase t))
  (with-open-file (in pathname)
    (if upcase
        (peruse-as-plist in #'string-upcase)
        (peruse-as-plist in))))

(defun set-data-to-text (pathname data)
  (with-open-file (out pathname :direction :output :if-exists :supersede)
    (prin1 (indite data) out)))
