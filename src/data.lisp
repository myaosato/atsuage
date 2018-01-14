(in-package :cl-user)
(defpackage atsuage.data
  (:use :cl)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :atsuage.files
                :get-text-path)
  (:import-from :atsuage.text
                :get-data-from-text
                :set-data-to-text)
  (:export :get-value
           :get-value-as-seq
           :set-value
           :add-value
           :make-data
           :save-data))
(in-package :atsuage.data)

(defun get-key (name)
  (read-from-string (format nil ":~A" (string-upcase name))))

;;; MANAGE DATA
(defvar *data-table* (make-hash-table :test #'equal))       

(defun load-data (name)
  (unless (data-exists name)
    (push-data name (get-data-from-text (get-text-path name)))))

(defun push-data (name data)
  (setf (gethash name *data-table*) data))

(defun data-exists (name)
  (second (multiple-value-list (gethash name *data-table*))))

(defmacro get-data (name)
  `(gethash ,name *data-table*))

;;; NEW DATA
(defun make-data (name)
  (push-data name nil))

;;; GET VALUE
(defun get-value-as-seq (prop name)
  (load-data name)
  (getf (get-data name) (get-key prop)))

(defun get-value (prop name &optional (ind 0))
  (let ((seq (get-value-as-seq prop name)))
    (if (and seq (> (length seq) ind))
        (elt seq ind))))

;; SET AND SAVE
(defun set-value (prop name obj &optional (save? nil))
  (if (data-exists name)
      (flet ((make-value (obj)
               (if (and (typep obj 'sequence) (not (stringp obj)))
                   (coerce obj 'vector)
                   (coerce (list (string obj)) 'vector))))
        (let ((value (make-value obj)))
          (setf (getf (gethash name *data-table*) prop) value)
          (if save? (save-data name))))))
  
(defun add-value (prop name str &optional (save? nil))
  (let ((value (string str))
        (seq (get-value-as-seq prop name)))
    (if seq
        (set-value prop name (append (coerce seq 'list) (list value)) save?)
        (set-value prop name value save?))))

  
(defun save-data (name)
  (if (data-exists name)
      (set-data-to-text (get-text-path name) (get-data name))))
