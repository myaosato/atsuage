(defpackage atsuage.core
  (:use :cl)
  (:import-from :cl-fad
                :pathname-as-directory)
  (:import-from :rosa
                :indite)
  (:import-from :atsuage.files
                :set-project-dirs
                :get-text-list
                :get-template-list
                :get-text-path
                :get-page-path
                :get-template-path
                :get-atsuage-path)
  (:import-from :atsuage.data
                :get-value-as-seq
                :set-value
                :add-value
                :make-data
                :save-data
                :get-key)
  (:import-from :atsuage.converter
                :convert
                :read-template-form-file)
  (:export :make-project
           :find-atsuage-dir
           :initialize           
           :make-text
           :make-page
           :get-text-list
           :get-template-list
           :get-config
           :get-key))

(in-package :atsuage.core)

(defvar *utf-8*
    #+(or sbcl ccl cmu allegro ecl lispworks) :utf-8
    #+clisp charset:utf-8)

;;; UTIL
(defun pwd ()
  (truename "."))

(defun mkdir (dir)
  (ensure-directories-exist (pathname-as-directory dir)))

(defun make-file (path str)
  (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from make-file nil))))
    (with-open-file (out path :direction :output
                         :if-exists :error
                         :if-does-not-exist :create
                         :external-format *utf-8*)
      (princ str out)
      t)))

(defun write-file (path str)
    (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from write-file nil))))
      (with-open-file (out path :direction :output :if-exists :supersede :external-format *utf-8*)
        (princ str out)
        t)))

;;; MAKE-PROJECT
(defun input-prompt (prompt)
  (format t "~A: " prompt)
  (force-output)
  (read-line))

(defvar *sample-text* 
  (list :TITLE "sample page" :DATE "" :UP "" :PREV "" :NEXT "" :TEXT "### Hello!!

* foo
* bar
* baz
"))

(defvar *default-text* 
  (list :TITLE "title" :DATE "" :UP "" :PREV "" :NEXT "" :TEXT "please write
...
...
"))

(defvar *sample-template* "(:html (:head (:title (get-value \"site-name\" \"project\")))
       (:body (:h1 (get-value :site-name \"project\"))
              (:p \"Welcome \" (get-value \"author\" \"project\") \"'s site\")
              (:h2 (get-value :title))
              (:main (get-value-as-md \"text\"))))")

(defun make-project (name &optional (dir (pwd)))
  (let* ((project-dir (merge-pathnames (pathname-as-directory name) (pathname-as-directory dir)))
         site-name author)
    (setf site-name (input-prompt "site-name"))
    (setf author (input-prompt "author"))
    (set-project-dirs project-dir)
    (mkdir project-dir)
    (make-file (get-atsuage-path) "")
    (mkdir (get-text-path ""))
    (mkdir (directory-namestring (get-page-path "")))
    (mkdir (get-template-path ""))
    (write-file (get-atsuage-path) 
                (write-to-string (list :ignore '("project") 
                                       :text-format (list :default *default-text*))))
    (make-text "project" (list :site-name site-name :author author))
    (make-file (get-template-path "template") (format nil "~A" *sample-template*))
    (make-text "index" *sample-text*)))

;;; FIND-PROJECT
(defun parent-dir (dir)
  (truename (merge-pathnames "../" (pathname-as-directory dir))))

(defun find-file-up (filename &optional (dir (pwd)))
  (cond ((probe-file (merge-pathnames filename (pathname-as-directory dir)))
         dir)
        ((= (length (pathname-directory (truename dir))) 1)
         nil)
        (t 
         (find-file-up filename (parent-dir dir)))))

(defun find-atsuage-dir ()
  (find-file-up ".atsuage"))

;;; INIT
(defun initialize (dir)
  (prepare-project dir))

(defvar *config*)

(defun read-config ()
  (let ((*read-eval* nil))
    (with-open-file (in (get-atsuage-path))
      (setf *config* (read in)))))

(defun get-config ()
  *config*)

(defun prepare-project (dir)
  (set-project-dirs dir)
  (read-config))

;;; MAKE-FILES
(defun read-template (template-name)
  (read-template-form-file (get-template-path template-name)))

(defun make-text (name &optional (key-strs *default-text*))
  (unless key-strs (setf key-strs *default-text*)) ;;
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

