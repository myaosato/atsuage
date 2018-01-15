(defpackage atsuage
  (:use :cl)
  (:import-from :atsuage.core
                :make-project
                :find-atsuage-dir
                :initialize           
                :make-text
                :make-page
                :get-text-list
                :get-template-list
                :get-config)
  (:export :command))

(in-package :atsuage)

(defun find-project ()
  (let ((dir (find-atsuage-dir)))
    (if dir
        (initialize dir))
    dir))

(defun %make-page-with-template (name template)
  (make-page name template))

(defun %make-page (name)
  (make-page name))

(defun command (&rest args)
  (find-project)
  (let ((command (car args)))
    (cond ((string= command "page")
           (%make-page (cadr args)))
          ((string= command "dir")
           (format t "~A" dir))
          (t nil))))
  
