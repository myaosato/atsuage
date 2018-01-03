(defpackage usuage.core
  (:use :cl)
  (:import-from :cl-fad
                :pathname-as-directory)
  (:import-from :rosa
                :indite)
  (:import-from :usuage.files
                :set-project-dirs
                :get-text-list
                :get-template-list
                :get-text-path
                :get-page-path
                :get-template-path)
  (:import-from :usuage.data
                :get-value-as-seq
                :set-value
                :add-value
                :make-data
                :save-data)
  (:import-from :usuage.converter
                :convert)
  (:export :initialize
           :make-project
           :make-text
           :make-page))

(in-package :usuage.core)

;;; UTIL
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

(defun write-file (path str)
    (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from write-file nil))))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (princ str out)
        t)))

;;; MAKE-PROJECT
(defun make-project (name &optional (dir (pwd)))
  (let* ((project-dir (merge-pathnames (pathname-as-directory name) (pathname-as-directory dir))))
    (set-project-dirs project-dir)
    (mkdir project-dir)
    (mkdir (get-text-path ""))
    (mkdir (get-page-path ""))
    (mkdir (get-template-path ""))))

;;; INIT
(defun initialize (dir)
  (prepare-project dir))

(defun prepare-project (dir)
  (set-project-dirs dir))

;;; MAKE-FILES
(defun read-template (template-name)
  (with-open-file (in (get-template-path template-name))
    (read in)))

(defun make-text (name &rest key-strs)
  (make-data name)
  (do* ((remain (reverse key-strs) (cddr remain))
        (obj (car remain) (car remain))
        (key (cadr remain) (cadr remain)))
       ((null remain) nil)
    (set-value key name obj))
  (save-data name))

(defun make-page (name &optional (template-name "template") (pre nil) (post nil))
  (if (functionp pre)
      (apply pre name template-name))
  (write-file (get-page-path name) (convert name (read-template template-name)))
  (if (functionp post)
      (apply post name template-name)))
