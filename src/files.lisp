(in-package :cl-user)
(defpackage usuage.files
  (:use :cl)
  (:import-from :cl-fad
                :pathname-as-directory
                :list-directory)
  (:import-from :alxandria
                :hash-table-plist
                :plist-hash-table)
  (:import-from :local-time)
  (:export :set-project
           :get-text-path
           :get-html-path
           :get-template-path
           :get-setting-file-path
           :get-project-file-path
           :get-template-names-path
           :register-time
           :is-registered
           :get-text-list))
(in-package :usuage.files)

(defvar *project-dir*)
(defvar *text-dir*)
(defvar *page-dir*)
(defvar *template-dir*)
(defvar *time-list-table*)

;;; SETTING
(defun set-project (dir)
  (setf *project-dir* (pathname-as-directory dir))
  (setf *text-dir* (merge-pathnames "text/" *project-dir*))
  (setf *page-dir* (merge-pathnames "page/" *project-dir*))
  (setf *template-dir* (merge-pathnames "template/" *project-dir*))
  (setf *time-list-table* (get-time-list-table)))

;;; PATH
(defun get-text-path (name)
  (merge-pathnames name *text-dir*))

(defun get-html-path (name)
  (merge-pathnames (format nil "~A.html" name) *html-dir*))

(defun get-template-path (name) 
  (merge-pathnames name *template-dir*))

(defun get-setting-file-path () 
  (merge-pathnames #p".usuage" *project-dir*))

(defun get-project-file-path () 
  (merge-pathnames #p"project" *project-dir*))

(defun get-template-names-path () 
  (merge-pathnames #p".template-name" *template-dir*))

(defun get-time-list-teble-path ()
  (merge-pathnames #p".time-list" *project-dir*))

;;; TIME
(defun get-time-list-table ()
  (with-open-file (in (get-time-list-teble-path))
    (plist-hash-table (read in) :test 'equal))

(defun get-registered-time (name)
  (gethash name *time-list-table*))

(defun get-time (name)
  (file-write-date (get-text-path name)))

(defun is-registered (name)
  (= (get-time name)
     (get-registered-time name)))

(defun register-time (name)
  (setf (gethash name *time-list-table*) (get-time name))
  (with-open-file (out (get-time-list-teble-path) 
                       :direction :output :if-exists :supersede)
    (prin1 (hash-table-plist *time-list-table*))))

;;; FILE-LIST  
(defun dir-p (pathname)
  (and (not (pathname-name pathname))))

(defun dir-name (pathname)
  (let ((path-list (pathname-directory pathname)))
    (format nil "~A/" (car (last path-list)))))

(defun ls (pathname)
  (setf pathname (pathname-as-directory pathname))
  (loop for pn in (list-directory pathname)
     collect (if (pathname-name pn) (file-namestring pn) (dir-name pn))))

(defun files (pathname)
  (setf pathname (pathname-as-directory pathname))
  (loop for pn in (list-directory pathname)
     append (if (pathname-name pn)
                (list (file-namestring pn))
                (mapcar (lambda (elt)
                          (concatenate 'string (dir-name pn) elt))
                        (deep-ls pn)))))

(defun get-text-list ()
  (files *text-dir*))
