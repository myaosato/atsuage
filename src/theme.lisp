(defpackage atsuage.theme
  (:use :cl)
  (:import-from :cl-fad
                :copy-file
                :pathname-as-directory)
  (:import-from :atsuage.utils
                :copy-dir-children)
  (:export :copy-theme
           :make-theme))

(in-package :atsuage.theme)

(defun copy-theme (dir from)
  (setf dir (pathname-as-directory dir))
  (setf from (pathname-as-directory from))
  (copy-dir-children (merge-pathnames #p"css/" from) (merge-pathnames #p"pages/css/" dir))
  (copy-dir-children (merge-pathnames #p"js/" from) (merge-pathnames #p"pages/js/" dir))
  (copy-dir-children (merge-pathnames #p"templates/" from) (merge-pathnames #p"templates/" dir))
  (copy-file (merge-pathnames #p".atsuage" from) (merge-pathnames #p".atsuage" dir)  :overwrite t))

(defun make-theme (dir)
  (setf dir (pathname-as-directory dir))
  (setf from (merge-pathnames #p"theme/" dir))
  (copy-dir-children (merge-pathnames #p"pages/css/" dir) (merge-pathnames #p"css/" from))
  (copy-dir-children (merge-pathnames #p"pages/js/" dir) (merge-pathnames #p"js/" from))
  (copy-dir-children (merge-pathnames #p"templates/" dir) (merge-pathnames #p"templates/" from))
  (copy-file (merge-pathnames #p".atsuage" dir) (merge-pathnames #p".atsuage" from)  :overwrite t))
