(in-package :cl-user)
(defpackage atsuage.files
  (:use :cl)
  (:import-from :cl-fad
                :pathname-as-directory
                :list-directory)
  (:import-from :alexandria
                :hash-table-plist
                :plist-hash-table)
  (:export :set-project-dirs
           :get-text-path
           :get-page-path
           :get-template-path
           :get-atsuage-path
           :get-update-time-path
           :get-text-list
           :get-template-list))
(in-package :atsuage.files)

(defvar *project-dir*)
(defvar *atsuage-file*)
(defvar *update-time-file*)
(defvar *texts-dir*)
(defvar *pages-dir*)
(defvar *templates-dir*)

;;; SETTING
(defun set-project-dirs (dir)
  (setf *project-dir* (pathname-as-directory dir))
  (setf *atsuage-file* (merge-pathnames ".atsuage" *project-dir*))
  (setf *update-time-file* (merge-pathnames ".update-time" *project-dir*))
  (setf *texts-dir* (merge-pathnames "texts/" *project-dir*))
  (setf *pages-dir* (merge-pathnames "pages/" *project-dir*))
  (setf *templates-dir* (merge-pathnames "templates/" *project-dir*)))

;;; PATH
(defun get-text-path (name)
  (merge-pathnames name *texts-dir*))

(defun get-page-path (name)
  (merge-pathnames (format nil "~A.html" name) *pages-dir*))

(defun get-template-path (name) 
  (merge-pathnames name *templates-dir*))

(defun get-atsuage-path ()
  *atsuage-file*)

(defun get-update-time-path ()
  *update-time-file*)

;;; FILE-LIST-UTIL  
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
                        (files pn)))))

;;; FILE-LIST
(defun get-text-list ()
  (files *texts-dir*))

(defun get-template-list ()
  (files *templates-dir*))
