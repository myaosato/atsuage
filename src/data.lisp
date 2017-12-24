(in-package :cl-user)
(defpackage usuage.data
  (:use :cl)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :usuage.files
                :get-texts-path)
  (:import-from :usuage.text
                :get-data-from-text
                :set-data-to-text)
  (:export :get-value
           :get-value-as-seq
           :set-value
           :add-value
           :make-data
           :save-data
           :set-current-name
           :get-current-name))
(in-package :usuage.data)

(defun get-key (name)
  (read-from-string (format nil ":~A" (string-upcase name))))

;;; CURRENT
(defvar *current-name*)

(defun set-current-name (name)
  (setf *current-name* name))

(defun get-current-name ()
  *current-name*)

;;; MANAGE DATA
(defvar *data-table* '(make-hash-table :test #'equal))       

(defun push-data (name data)
  (setf (gethash name *data-table*) data))

(defun exists-data (name)
  (gethash name *data-table*))

(defmacro seq-p (obj)
  `(typep ,obj 'sequence))

(defun get-data (&optional name)
  (let ((data (exists-data name)))
    (if data
        data
        (push-data name (get-data-from-text name)))))

;;; NEW DATA
(defun make-data (name)
  (push-data name nil))

;;; GET VALUE
(defun get-value-as-seq (prop &optional (name *current-name*))
  (getf (get-data name) (get-key prop)))

(defun get-value (prop &optional (name *current-name*) (ind 0))
  (let ((seq (get-value-as-seq prop name)))
    (if (and seq (> (length seq) ind))
        (elt seq ind))))

;; SET AND SAVE
(defun set-value (prop name obj &optional (save? nil))
  (flet ((make-value (obj)
           (if (seq-p obj)
               (coerce obj 'vector)
               (coerce (list (string obj)) 'vector))))
    (let ((data (get-data name))
          (value (make-value obj)))
      (setf (gethash prop data) obj)
      (if save? (save-data name)))))
  
(defun add-value (prop name str &optional (save? nil))
  (let ((value (string str))
        (seq (get-value-as-seq prop name)))
    (if seq
        (set-value prop name (append (coerce seq 'list) (list str)) save?)
        (set-value prop name str save?))))

  
(defun save-data (name)
  (let ((data (exists-data name)))
    (if data
        (set-data-to-text (get-text-path name) data))))
