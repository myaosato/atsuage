(defpackage atsuage.config
  (:use :cl)
  (:import-from :atsuage.utils
                :get-key)
  (:import-from :atsuage.files
                :get-atsuage-path)
  (:export :read-config
           :get-config
           :ignore-p
           :get-text-format
           :get-default-templates))

(in-package :atsuage.config)

(defvar *config*)

(defun read-config ()
  (let ((*read-eval* nil))
    (with-open-file (in (get-atsuage-path))
      (setf *config* (read in)))))

(defun get-config ()
  *config*)

(defun ignore-p (name)
  (find name (getf (get-config) :ignore) :test #'equal))

(defun get-text-format (name)
  (getf (getf (get-config) :text-format) (get-key (if name name "default"))))

(defun get-default-templates ()
  (getf *config* :default-templates))
