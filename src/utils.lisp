(defpackage atsuage.utils
  (:use :cl)
  (:import-from :cl-fad
                :list-directory
                :pathname-as-directory
                :copy-file)
  (:export :get-key
           :cwd
           :mkdir
           :make-file
           :write-file
           :input-prompt
           :copy-dir-children
           :*utf-8*))

(in-package :atsuage.utils)


;; Variables

(defvar *utf-8*
    #+(or sbcl ccl cmu allegro ecl lispworks) :utf-8
    #+clisp charset:utf-8)

;; Functions

(defun input-prompt (prompt)
  (format t "~A " prompt)
  (force-output)
  (read-line))

(defun get-key (name)
  (read-from-string (format nil ":~A" (string-upcase name))))

(defun cwd ()
  (truename "."))

(defun mkdir (dir)
  (ensure-directories-exist (pathname-as-directory dir)))

(defun make-file (path str)
  (ignore-errors
    (with-open-file (out path :direction :output :if-exists :error :if-does-not-exist :create 
                         :external-format *utf-8*)
      (princ str out)
      path)))

(defun write-file (path str)
  (ignore-errors
   (with-open-file (out path :direction :output :if-exists :supersede 
                        :external-format *utf-8*)
     (princ str out)
     path)))

(defun copy-dir-children (from to)
  (setf from (pathname-as-directory from))
  (setf to (pathname-as-directory to))
  (ensure-directories-exist to)
  (let ((targets (remove-if (lambda (e) (String= (file-namestring e) ""))
                            (list-directory from))))
    (loop for file in targets 
          do (copy-file file (merge-pathnames (file-namestring file) to) :overwrite t))))




