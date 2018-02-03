(in-package :cl-user)
(defpackage atsuage.converter
  (:use :cl)
  (:import-from :cl-markdown
                :markdown)
  (:import-from :atsuage.data
                :get-value
                :get-value-as-seq)
  (:export :convert
           :read-template-form-file))
(in-package :atsuage.converter)

;;; CURRENT
(defvar *current-name*)

(defun set-current-name (name)
  (setf *current-name* name))

(defun get-current-name ()
  *current-name*)

;; ****
(defvar *empty-elements* (make-hash-table))
(defvar *empty-elements-list* 
   '(:br :img :hr :meta :input :embed :area :base :col :keygen :link :param :source))
(dolist (tag *empty-elements-list*)
  (setf (gethash tag *empty-elements*) t))
(defun empty-element-p (key)
  (gethash key *empty-elements*))

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

;; HTML-TAG-STRING
(defun make-empty-element-string (tag attr-list)
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
  (if (empty-element-p tag)
      (make-empty-element-string tag attr-list)
      (make-element-string tag attr-list inner-html)))

;; CONVERTER
(defun convert (name template-sexp)
  (set-current-name name)
  (format nil "~A~A"
          "<!doctype html>"
          (htmlisp template-sexp)))

(defun read-template-form-file (template-path)
  (let ((*read-eval* nil))
    (with-open-file (in template-path)
      (let ((*package* (find-package :atsuage.converter)))
        (read in)))))

(defun read-template-form-string (template-string)
  (let ((*read-eval* nil))
    (let ((*package* (find-package :atsuage.converter)))
      (read-from-string template-string))))

;; HTMLISP
(defvar *htmlisp-functions* (make-hash-table))

(defvar *htmlisp-macros* (make-hash-table))

(defmacro def-hl-fun (name args &body body) 
  `(setf (gethash ',name *htmlisp-functions*) (lambda ,args ,@body)))

(defmacro def-hl-macro (name args &body body) 
  `(setf (gethash ',name *htmlisp-macros*) (lambda ,args ,@body)))

(defun hl-fun-p (name)
  (gethash name *htmlisp-functions*))

(defun hl-macro-p (name)
  (gethash name *htmlisp-macros*))

(defun call-hl-fun (name args-list-sexp mode vars)
  (apply (gethash name *htmlisp-functions*) (mapcar #'(lambda (s-exp) (htmlisp s-exp mode vars)) args-list-sexp)))

(defun expand-eval-hl-macro (name args-list-sexp mode vars)
  (let ((eval-s-exp (apply (gethash name *htmlisp-macros*) args-list-sexp)))
    (htmlisp eval-s-exp mode vars)))

(defun find-var (sym vars)
  (if (null vars) (return-from find-var (list nil nil)))
  (let ((var (multiple-value-list (gethash sym (car vars)))))
    (if (second var)
        var
        (find-var sym (cdr vars)))))

;; HTMLISP-CORE
(defun htmlisp (s-exp &optional (mode 'html) (vars nil))
  ;(print s-exp) for debug, myabe i will impliment log-mode
  (cond ((symbolp s-exp) ; symbol -> symbol, if symbol si registered as variables symbol -> value 
         (let ((var (find-var s-exp vars)))
           (if (second var)
               (first var)
               s-exp)))
        ((stringp s-exp) ; string -> string
         s-exp)
        ((and (listp s-exp) (keywordp (car s-exp)) (eq mode 'html)) ; html elements
         (if (eq (cadr s-exp) '&) ; attributes exist
             (make-element (car s-exp) (htmlisp (caddr s-exp) 'attr vars) (concat-htmls (cdddr s-exp) 
                                                                                        mode vars))
             (make-element (car s-exp) nil (concat-htmls (cdr s-exp) mode vars))))
        ((and (listp s-exp) (keywordp (car s-exp)) (eq mode 'attr)) ; attributes
         (mapcar #'(lambda (s-exp) (htmlisp s-exp mode vars)) s-exp))
        ((and (listp s-exp) (eq 'set-vars (car s-exp))) ; like let
         (set-vars (cadr s-exp) (cddr s-exp) mode vars))
        ((and (listp s-exp) (hl-fun-p (car s-exp))) ; hl-fun
         (call-hl-fun (car s-exp) (cdr s-exp) mode vars))
        ((and (listp s-exp) (hl-macro-p (car s-exp))) ; hl-macro
         (expand-eval-hl-macro (car s-exp) (cdr s-exp) mode vars)) 
        (t "")))

(defun concat-htmls (html-list mode vars)
  (if (null html-list)
      nil
      (format nil "~{~A~}" (mapcar #'(lambda (s-exp) (htmlisp s-exp mode vars)) html-list))))

(defun set-vars (var-list body mode vars)
  (let ((vars-hash (make-hash-table))
        (local-vars))
    (dolist (var-val var-list)
      (setf (gethash (car var-val) vars-hash) (cadr var-val)))
    (setf local-vars (cons vars-hash vars))
    (format nil "~{~A~}" (mapcar #'(lambda (s-exp) (htmlisp s-exp mode local-vars)) body))))


;; HTMLISP-FUNCTIONS
(def-hl-fun get-value (prop &optional name)
  (if (null name)
      (setf name (get-current-name)))
      (get-value prop name))

(def-hl-fun get-value-as-md (prop  &optional name) 
  (if (null name)
      (setf name (get-current-name)))
  (second (multiple-value-list (markdown (get-value prop name) :stream nil))))

(def-hl-fun get-value-as-list (prop  &optional name) 
  (if (null name)
      (setf name (get-current-name)))
  (concatenate 'list (get-value-as-seq prop name)))

(def-hl-fun concat (&rest strs) 
  (format nil "~{~A~}" strs))

(def-hl-fun each (func lst)
  (format nil "~{~A~}" (mapcar func lst)))

(def-hl-fun anchor (href label)
  (htmlisp (list :a (list :href href) label)))

;; HTMLISP-MACROS
(def-hl-macro collect (var lst-sexp &rest body)
  (let ((lst (htmlisp lst-sexp))
        (result))
    (dolist (val lst)
      (push `(set-vars ((,var ,val)) ,@body) result))
    (cons 'concat (reverse result))))

