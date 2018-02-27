(defpackage atsuage
  (:use :cl)
  (:import-from :atsuage.core
                :make-project
                :find-atsuage-dir
                :initialize           
                :end
                :convertedp
                :make-text
                :make-page
                :auto-update
                :get-text-list
                :get-template-list
                :get-config
                :get-key
                :pwd)
  (:export :command
           :repl-command))

(in-package :atsuage)

(defun find-project (current-dir)
  (let ((dir (find-atsuage-dir current-dir)))
    (if dir
        (initialize dir))
    dir))

(defun ignore-p (name)
  (find name (getf (get-config) :ignore) :test #'equal))

(defun get-text-format (name)
  (if (null name) (return-from get-text-format nil))
  (getf (getf (get-config) :text-format) (get-key name)))

(defvar *help*
  (format nil "
atsuage 
  
  version ~A

  simple static site generator 

  new-project [name] : make new project
  new [name] : make new text
  new [name] [format] : make new text using specified format
  inew [name] : make new text (interactive)
  inew [name] [format] : make new text using specified format (interactive)
  page [name] : make page and resolve relation
  page [name] [template] : make page using specified template and resolve relation
  update : make pages and resolve relation (updated texts only)
  page-all : make pages (all texts) this dosen't resolve relation
  refresh : make pages (all texts) this resolve relation
  dir : show current project directry
  texts : show text list
  conf : show config
  updated : show updated text files
  help : show help message
" (slot-value (asdf:find-system :atsuage) 'asdf:version)))

(defun help ()
  (format t "~A~%" *help*))

(defun new-project (name)
  (make-project name))

(defun page (name &optional template-name)
  (if template-name
      (make-page name :template-name template-name)
      (make-page name))
  (auto-update name))
  
(defun update ()
  (let ((lst (remove-if (lambda (name) (or (ignore-p name) (convertedp name))) (get-text-list))))
    (loop for name in lst
          do (progn
               (make-page name)
               (auto-update name)))))

(defun page-all ()
  (let ((lst (remove-if #'ignore-p (get-text-list))))
    (loop for name in lst
          do (make-page name))))

(defun refresh ()
  (let ((lst (remove-if #'ignore-p (get-text-list))))
    (loop for name in lst
          do (progn (make-page name)
                    (auto-update name)))))

(defun updated ()
  (format t "~{~A~%~}" (remove-if (lambda (name) (or (ignore-p name) (convertedp name))) (get-text-list))))

(defun new (name &optional text-format)
  (if text-format
      (make-text name nil (get-text-format text-format))
      (make-text name nil (get-text-format "default"))))

(defun inew (name &optional text-format)
  (if text-format
      (make-text name t (get-text-format text-format))
      (make-text name t (get-text-format "default"))))


(defun command (args &optional current-dir)
  (let ((command (car args))
        (dir (find-project current-dir)))
    (cond ((null command)
           (help))
          ((string= command "new-project")
           (new-project (cadr args)))
          ((string= command "help")
           (help))
          ((null dir)
           (format t "can't find an atsuage project~%"))
          (t 
           (cond ((string= command "new")
                  (new (cadr args) (caddr args)))
                 ((string= command "inew")
                  (inew (cadr args) (caddr args)))
                 ((string= command "page")
                  (page (cadr args) (caddr args)))
                 ((string= command "update")
                  (update))
                 ((string= command "page-all")
                  (page-all))
                 ((string= command "refresh")
                  (refresh))
                 ((string= command "dir")
                  (format t "~A~%" dir))
                 ((string= command "texts")
                  (format t "~A~%" (get-text-list)))
                 ((string= command "conf")
                  (format t "~S~%" (get-config)))
                 ((string= command "updated")
                  (updated))
                 (t 
                  (format t "command not found: atsuage~{ ~A~}~%" args)))
           (end)))))

(defun repl-command (command dir &rest args)
  (unless (stringp command)
    (setf command (string-downcase (string command))))
  (command (cons command args)) dir)
