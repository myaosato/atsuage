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
                :get-time-list-teble-path
                :register-time
                :is-registered
                :get-text-list)
  (:import-from :usuage.data
                :read-template
                :load-template-names-file
                :load-setting-file)
  (:import-from :usuage.text
                :set-data-to-text)
  (:import-from :usuage.converter
                :convert)
  (:export :make-project
           :make-text
           :make-page
           :update
           :update-all))
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

;; 
(defun make-project (name &optional (dir (pwd)))
  (let* ((project-dir (merge-pathnames (pathname-as-directory name) dir)))
    (set-project-dirs project-dir)
    (mkdir project-dir)
    (mkdir (get-text-path ""))
    (mkdir (get-page-path ""))
    (mkdir (get-template-path ""))
    (make-file (get-project-file-path) (indite '(:title #("") :author #("") :since #(""))))
    (make-file (get-template-names-path) "")
    (make-file (get-time-list-teble-path) "")
    (make-file (get-setting-file-path) "")))

;;
(defun find-project () nil) ;; TODO

(defun init () nil) ;; TODO

(defun prepare-project (dir)
  (set-project-dirs dir)
  (set-time-list-table dir)
  (load-setting-file)
  (load-template-names-file))

;;
(defun make-text (name &optional (title "") (date "")
                         (prev "") (next "") (up "") (id "") (text ""))
  (set-data-to-text (get-text-path name)
                    (loop for remain
                       on (list :title title :date date
                                :prev prev :next next :up up :id id :text text)
                       by #'cddr
                       append (list (car remain)
                                    (make-array 1 :initial-element (cadr remain))))))

(defun make-page (name)
  (let ((template-sexp (read-template name)))
    (with-open-file (out (get-page-path name)
                         :direction :output
                         :if-exists :supersede)
      (princ (convert name template-sexp) out))
    (register-time name)))

(defun update-if-not (predicate)
  (loop for name in (remove-if predicate (get-text-list))
       do (make-page name)))

(defun update ()
  (update-if-not #'is-registerd))

(defun update-all ()
  (update-if-not (lambda (elt) (declare (ignore elt)) nil)))




  



