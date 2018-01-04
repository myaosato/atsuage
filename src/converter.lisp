(in-package :cl-user)
(defpackage usuage.converter
  (:use :cl)
  (:import-from :cl-markdown
                :markdown)
  (:import-from :usuage.data
                :get-value
                :get-value-as-seq)
  (:export :convert))
(in-package :usuage.converter)

;;; CURRENT
(defvar *current-name*)

(defun set-current-name (name)
  (setf *current-name* name))

(defun get-current-name ()
  *current-name*)

;; ****
(defvar *no-end-tags* (make-hash-table))
(defvar *no-end-tag-list* 
   '(:br :img :hr :meta :input :embed :area :base :col :keygen :link :param :source))
(dolist (tag *no-end-tag-list*)
  (setf (gethash tag *no-end-tags*) t))
(defun no-end-tag-p (key)
  (gethash key *no-end-tags*))

;; ****
(defun nil-to-blank (obj)
  (if (null obj)
      ""
      obj))

;; HTML-ATTR-STRING  
(defun convert-list-to-attribute-format (attr-list &optional (str ""))
  (cond ((null attr-list) str)
        ((symbolp (car attr-list))
         (cond ((stringp (cadr attr-list))
                (%convert-list-to-attribute-format 
                 (cddr attr-list)
                 (format nil 
                         "~A ~A=\"~A\"" 
                         str
                         (string-downcase (string (car attr-list)))
                         (cadr attr-list))))
               (t
                (%convert-list-to-attribute-format 
                 (cddr attr-list)
                 (format nil 
                         "~A ~A" 
                         str
                         (string-downcase (string (car attr-list))))))))
        (t str)))

;; HTML-TAG-STRING
(defun make-no-end-tag-element-string (tag attr-list)
  (let ((tag-name (string-downcase (string tag))))
    (format nil "<~A~A />" tag-name (convert-list-to-attribute-format attr-list))))

(defun make-element-string (tag attr-list inner-html)
  (let ((tag-name (string-downcase (string tag))))
    (format nil "<~A~A>~A</~A>" 
            tag-name
            (convert-list-to-attribute-format attr-list)
            (nil-to-blank inner-html)
            tag-name)))

(defun make-element (tag attr-list inner-html)
  (if (no-end-tag-p tag)
      (make-no-end-tag-element-string tag attr-list)
      (make-element-string tag attr-list inner-html)))


;; CONVERTER
(defun convert (name template-sexp)
  (set-current-name name)
  (let ((*package* (find-package :usuage.converter)))
    (htmlisp template-sexp))


;; HTMLISP
(defvar *htmlisp-functions* (make-hash-table))

(defmacro def-hl-fun (name args &body body) 
  `(setf (gethash ',name *htmlisp-functions*) (lambda ,args ,@body)))

(defun hl-fun-p (name)
  (gethash name *htmlisp-functions*))

(defun call-hl-fun (name args)
  (apply (gethash name *htmlisp-functions*) args))

;; HTMLISP-CORE
(defun htmlisp (s-exp &optional (mode 'html))
  (cond ((symbolp s-exp)
         s-exp)
        ((stringp s-exp)
         s-exp)
        ((listp s-exp)
         (cond ((keywordp (car s-exp))
                (cond ((eq mode 'html)
                       (let ((attr-list nil)
                             (inner-html nil))
                         (cond ((eq (cadr s-exp) '&)
                                (setf attr-list (htmlisp (caddr s-exp) 'attr))
                                (setf inner-html (concat-htmls (cdddr s-exp))))
                               (t
                                (setf inner-html (concat-htmls (cdr s-exp)))))
                         (make-element (car s-exp)
                                       attr-list
                                       inner-html)))
                      ((eq mode 'attr)
                       (mapcar 'htmlisp s-exp))
                      (t "")))
               (t
                (cond ((hl-fun-p (car s-exp))
                       (call-hl-fun (car s-exp) (mapcar 'htmlisp (cadr s-exp))))
                      (t "")))))
        (t "")))

(defun concat-htmls (html-list)
  (if (null html-list)
      nil
      (format nil "~{~A~}" (mapcar #'htmlisp html-list))))

;; HTMLISP-FUNCTION
(def-hl-fun get-value (prop name)
  (if (null name)
      (setf name (get-current-name)))
      (get-value name prop))

(def-hl-fun get-value-as-md (prop name) 
  (if (null name)
      (setf name (get-current-name)))
  (markdown (get-value name prop)))


(def-hl-fun get-value-as-list (name prop) 
  (if (String= name "this")
      (setf name *current-file-name*))
  (concatenate 'list (get-prop-as-seq name prop)))

(def-hl-fun each (func lst)
  (format nil "~{~A~}" (mapcar func lst)))

(def-hl-fun anchor (href label)
  (htmlisp (list :a (list :href href) label)))


