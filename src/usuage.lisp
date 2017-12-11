(defpackage usuage
  (:use :cl)
  (:import-from :cl-fad
                :pathname-as-directory)
  (:import-from :rosa
                :indite)
  (:import-from :usuage.files
                :set-project-dirs
                :get-text-path
                :get-page-path
                :get-template-path
                :get-setting-file-path
                :get-project-file-path
                :get-template-names-path
                :get-time-list-teble-path)
  (:export :make-project))
(in-package :usuage)

(defun pwd ()
  (truename "."))

(defun mkdir (dir)
  (ensure-directories-exist (pathname-as-directory dir)))

(defun make-file (path str)
  (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from make-file nil))))
    (with-open-file (out path :direction :output
                         :if-exists :error
                         :if-does-not-exist :create)
      (princ str out)
      t)))

(defun make-project (name &optional (dir (pwd)))
  (let* ((project-dir (merge-pathnames (pathname-as-directory name) dir)))
    (set-project-dirs project-dir)
    (mkdir project-dir)
    (mkdir (get-text-path ""))
    (mkdir (get-page-path ""))
    (mkdir (get-template-path ""))
    (make-file (get-project-file-path) (indite '(:title "" :author "" :since "")))
    (make-file (get-template-names-path) "")
    (make-file (get-time-list-teble-path) "")
    (make-file (get-setting-file-path) "")))


  

