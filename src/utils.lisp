(defpackage atsuage.utils
  (:use :cl)
  (:export :get-key))

(in-package :atsuage.utils)

(defun get-key (name)
  (read-from-string (format nil ":~A" (string-upcase name))))
