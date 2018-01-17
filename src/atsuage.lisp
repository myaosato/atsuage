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

(defun %make-page (name)
  (make-page name))

(defun ignore-p (name)
  (find name (getf (get-config) :ignore) :test #'equal))

(defun make-all ()
  (let ((lst (remove-if #'ignore-p (get-text-list))))
    (loop for name in lst
          do (%make-page name))))

(defvar *help*
  (format nil "

atsuage 

  simple static site generator (WIP)

  new-prpject [name] : make new project
  page [name] : make page
  page [name] [template] : make page 
  all : make pages
  dir : show current project directry
  texts : show text list
  conf : show config
  help : show help message
"))

(defun command (args)
  (let ((command (car args))
        (dir (find-project)))
    (cond ((string= command "new-project")
           (make-project (cadr args)))
          ((string= command "help")
           (format t "~A~%" *help*))
          ((null dir)
           (format t "can't find an atsuage project~%"))
          ((string= command "page")
           (if (= (length args) 1)
               (%make-page (cadr args))
               (make-page (cadr args) (caddr args))))
          ((string= command "all")
           (make-all))
          ((string= command "dir")
           (format t "~A~%" dir))
          ((string= command "texts")
           (format t "~A~%" (get-text-list)))
          ((string= command "conf")
           (format t "~S~%" (get-config)))
          (t 
           (format t "command not found: atsuage~{ ~A~}~%" args)))))
  
