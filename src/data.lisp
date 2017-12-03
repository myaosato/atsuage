(in-package :cl-user)
(defpackage usuage.data
  (:use :cl)
  (:import-from :cl-ppcre
                :)
  (:import-from :usuage.files
                :get-text-path
                :get-setting-file-path
                :get-project-file-path
                :get-template-path
                :get-template-names-path)
  (:import-from :usuage.text
                :get-data-from-text
                :set-data-to-text)
  (:export :get-value
           :get-value-as-seq
           :set-value
           :add-value
           :save-data))

(in-package :usuage.data)

(defun get-key (name)
  (read-from-string (format nil ":~A" (string-upcase prop))))

;;; SETTING
(defvar *settitng-data*)

(defun load-setting-file ()
  (setf *settitng-data* (get-data-from-text (get-setting-file-path))))

(defun get-setting-value (prop)
  (elt (getf *settitng-data* (get-key prop)) 0)) 

;;; TEMPLATE
(defvar *template-name-data*)

(defun load-template-names-file ()
  (setf *template-name-data* (get-data-from-text (get-template-names-path) nil)))

(defun get-template-name (text-name)
  (do ((template-name nil)
       (ind 1 (+ ind 2)))
      ((or template-name
           (>= ind (length *template-name-data*)))
       template-name)
    (if (scan (concatenate 'stirng (elt *template-name-data* ind) "$") text-name)
        (setf template-name (elt *template-name-data* (1- ind))))))

(defun read-template (name)
  (with-open-file (in (get-template-path (get-template-name name)))
    (read in)))

;;; KEY-VALUE
(defvar *current-name*)

(defun set-current-name (name)
  (setf *current-name* name))

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
        (push-data name
                   (cond ((String= name "project")
                          (get-data-from-text (get-project-file-path)))
                         ((or (null name) (String= name ""))
                          (get-data-from-text (get-text-path *current-name*)))
                         (t (get-data-from-text (get-text-path name))))))))

(defun save-data (name)
  (let ((data (exists-data name)))
    (if data
        (%save-data name data))))

(defun %save-data (name data)
  (cond ((String= name "project")
         (set-data-to-text (get-project-file-path) data))
        (t (set-data-to-text (get-text-path name) data))))

(defun get-value-as-seq (prop name)
  (getf (get-data name) (get-key prop)))

(defun get-value (prop name &optional (ind 0))
  (let (seq (get-prop-as-seq prop name)))
    (if (and seq (> (length seq) ind))
        (elt seq ind)))

(defun set-value (prop name obj)
  (cond ((or (stringp obj) (seq-p obj))
         (setf (getf (get-data name) (get-key prop))
               (coerce (if (stringp obj) (list obj) obj) 'vect)))
        (t nil))
  (save-data name))
  
(defun add-value (prop name str)
  (if (not (stringp str) (return-from add-prop nil)))
  (let ((seq (get-prop-as-seq prop name)))
    (if seq
        (set-prop prop name (append (coerce seq 'list) (list str)))
        (set-prop porp name str)))
  (save-data name))





