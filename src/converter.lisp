(in-package :cl-user)
(defpackage usuage.converter
  (:use :cl))
(in-package :usuage.converter)

(eval-when (:load-toplevel :compile-toplevel)
  (defvar *no-end-tags* (make-hash-table))
  (defvar *no-end-tag-list* 
    '(:br :img :hr :meta :input :embed :area :base :col :keygen :link :param :source)))


(defun no-end-tag-p (key)
  (gethash key *no-end-tags*))

(defun convert-list-to-attribute-format (attr-list &optional (str ""))
  (cond ((null attr-list) str)
        ((symbolp (car attr-list))
         (cond ((stringp (cadr attr-list))
                (convert-list-to-attribute-format 
                 (cddr attr-list)
                 (format nil 
                         "~A ~A=\"~A\"" 
                         str
                         (string-downcase (string (car attr-list)))
                         (cadr attr-list))))
               (t
                (convert-list-to-attribute-format 
                 (cddr attr-list)
                 (format nil 
                         "~A ~A" 
                         str
                         (string-downcase (string (car attr-list))))))))
        (t str)))

(defun make-no-end-tag-element-string (tag attr-list)
  (let ((tag-name (string-downcase (string tag))))
    (format nil "<~A~A />" tag-name (convert-list-to-attribute-format attr-list))))

(defun make-element-string (tag attr-list inner-html)
  (let ((tag-name (string-downcase (string tag))))
    (format nil "<~A~A>~A</~A>" 
            tag-name (convert-list-to-attribute-format attr-list) inner-html tag-name)))
