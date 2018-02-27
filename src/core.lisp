(defpackage atsuage.core
  (:use :cl)
  (:import-from :cl-fad
                :pathname-as-directory)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :alexandria
                :plist-hash-table
                :hash-table-plist)
  (:import-from :rosa
                :indite)
  (:import-from :local-time
                :now
                :format-timestring
                :timestamp-to-unix)
  (:import-from :atsuage.files
                :set-project-dirs
                :get-text-list
                :get-template-list
                :get-text-path
                :get-page-path
                :get-template-path
                :get-atsuage-path
                :get-update-time-path)
  (:import-from :atsuage.data
                :get-value-as-seq
                :get-value
                :set-value
                :add-value
                :pushnew-value
                :make-data
                :save-data
                :get-key)
  (:import-from :atsuage.converter
                :convert
                :read-template-form-file)
  (:export :make-project
           :find-atsuage-dir
           :initialize           
           :end
           :convertedp
           :make-text
           :make-page
           :auto-update
           :get-text-list
           :get-template-list
           :get-config
           :get-key
           :pwd))

(in-package :atsuage.core)

(defvar *utf-8*
    #+(or sbcl ccl cmu allegro ecl lispworks) :utf-8
    #+clisp charset:utf-8)

;;; UTIL
(defun pwd ()
  (truename "."))

(defun mkdir (dir)
  (ensure-directories-exist (pathname-as-directory dir)))

(defun make-file (path str)
  (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from make-file nil))))
    (with-open-file (out path :direction :output
                         :if-exists :error
                         :if-does-not-exist :create
                         :external-format *utf-8*)
      (princ str out)
      t)))

(defun write-file (path str)
    (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from write-file nil))))
      (with-open-file (out path :direction :output :if-exists :supersede :external-format *utf-8*)
        (princ str out)
        t)))


;;; MAKE-PROJECT
(defun input-prompt (prompt)
  (format t "~A: " prompt)
  (force-output)
  (read-line))

(defvar *sample-text* 
  (list :TITLE "sample page" :DATE "" :PARENT "" :PREV "" :NEXT "" :TEXT "### Hello!!

* foo
* bar
* baz
"))

(defvar *default-text* 
  (list :TITLE "title" :DATE "" :PARENT "" :PREV "" :NEXT "" :TEXT "please write
...
...
"))

(defvar *sample-template* "(:html (:head (:title (get-value \"site-name\" \"project\")))
       (:body (:h1 (get-value :site-name \"project\"))
              (:p \"Welcome \" (get-value \"author\" \"project\") \"'s site\")
              (:h2 (get-value :title))
              (:main (get-value-as-md \"text\"))))")

(defun make-project (name &optional (dir (pwd)))
  (let* ((project-dir (merge-pathnames (pathname-as-directory name) (pathname-as-directory dir)))
         site-name author)
    (setf site-name (input-prompt "site-name"))
    (setf author (input-prompt "author"))
    (set-project-dirs project-dir)
    (mkdir project-dir)
    (make-file (get-atsuage-path) "")
    (mkdir (get-text-path ""))
    (mkdir (directory-namestring (get-page-path "")))
    (mkdir (get-template-path ""))
    (write-file (get-atsuage-path) 
                (write-to-string (list :ignore '("project") 
                                       :text-format (list :default *default-text*)
                                       :default-templates nil)))
    (make-text "project" (list :site-name site-name :author author))
    (make-file (get-template-path "template") (format nil "~A" *sample-template*))
    (make-text "index" *sample-text*)))

;;; FIND-PROJECT
(defun parent-dir (dir)
  (truename (merge-pathnames "../" (pathname-as-directory dir))))

(defun find-file-up (filename &optional (dir (pwd)))
  (cond ((probe-file (merge-pathnames filename (pathname-as-directory dir)))
         dir)
        ((= (length (pathname-directory (truename dir))) 1)
         nil)
        (t 
         (find-file-up filename (parent-dir dir)))))

(defun find-atsuage-dir (&optional dir)
  (if dir
      (find-file-up ".atsuage" dir)
      (find-file-up ".atsuage")))

;;; UPDATE-TIME
(defvar *update-time* (make-hash-table :test 'equal))

(defun load-update-time ()
  (if (probe-file (get-update-time-path)) 
      (with-open-file (in (get-update-time-path))
        (setf *update-time* (plist-hash-table (read in) :test 'equal)))))

(defun read-time (name)
  (gethash name *update-time*))

(defun convertedp (name)
  (let ((recoded-time (read-time name))
        (file-time (file-write-date (get-text-path name))))
    (and recoded-time (= recoded-time file-time))))

(defun write-time (name)
  (setf (gethash name *update-time*) (file-write-date (get-text-path name))))

(defun save-update-time ()
  (with-open-file (out (get-update-time-path) :direction :output :if-exists :supersede)
    (print (hash-table-plist *update-time*) out)))

;;; CONFIG FILE
(defvar *config*)

(defun read-config ()
  (let ((*read-eval* nil))
    (with-open-file (in (get-atsuage-path))
      (setf *config* (read in)))))

(defun get-config ()
  *config*)

;;; SELECT TEMPLATE
(defparameter *default-template-name* "template")

(defun find-template (name)
  (let ((template (find-if #'(lambda (elt) (string= (car elt) name)) 
                           (getf *config* :default-templates))))
    (if (stringp (cdr template))
        (cdr template)
        *default-template-name*))) ;;

;;; INITIALIZE
(defun initialize (dir)
  (set-project-dirs dir)
  (read-config)
  (load-update-time))

;;; END
(defun end ()
  (save-update-time))

;;; TIMESTAMP FORMAT
(defvar +date+ '((:year 4) #\- (:month 2) #\- (:day 2)))
(defvar +time+ '((:hour 4) #\- (:min 2) #\- (:sec 2)))
(defun get-date ()
  (format-timestring nil (now) :format +date+))
(defun get-time ()
  (format-timestring nil (now) :format +time+))
(defun get-unix ()
  (format nil "~A" (timestamp-to-unix (now))))

;;; GET-LAST-NAME
(defvar +date-regex+ "^\\d{4}(?:0[1-9]|1[0-2])(?:0[1-9]|[12][0-9]|3[01])(?:-\\d+)?$")

(defun get-last-name (regex-or-key)
  (let (regex)
    (cond ((eq regex-or-key :date) (setf regex +date-regex+))
          ((stringp regex-or-key) (setf regex regex-or-key))
          (t (setf regex "")))
    (car (sort (remove-if #'(lambda (name) (not (scan regex name))) (get-text-list)) #'string>))))

;;; GET-NAME
(defvar +date-as-name+ '((:year 4) (:month 2) (:day 2)))
(defun get-date-as-name ()
  (let ((tmp (format-timestring nil (now) :format +date-as-name+))
        (last-name (get-last-name +date-regex+)))
    (if (string= (subseq last-name 0 8) tmp)
        (if (> (length last-name) 8)
            (format nil "~A-~A" tmp (1+ (parse-integer (subseq last-name 9))))
            (format nil "~A-~A" tmp 1))
        tmp)))

;;; MAKE-TEXTS
(defun plist-keys (plist)
  (do ((result nil)
       (ind 0 (+ ind 2)))
      ((>= ind (length plist)) result)
    (push (elt plist ind) result)))

(defun get-default-value (data-plist key)
  (let ((val (getf data-plist key)))
    (cond ((eq val :date) 
           (get-date))
          ((eq val :time) 
           (get-date))
          ((eq val :unix) 
           (get-unix))
          ((eq val :universal) 
           (get-universal-time))
          ((and (consp val) (eq (car val) :last))
           (get-last-name (cadr val)))
          (t val))))

(defun make-text (name interactive-p &optional (data-plist *default-text*))
  (unless data-plist (setf data-plist *default-text*)) ;;
  ;; special name
  (cond ((string= "\\date" name)
         (setf name (get-date-as-name))))
  (make-data name)
  (if interactive-p
      (dolist (key (reverse (plist-keys data-plist))) ;;
        (if (and (stringp (getf data-plist key)) (find #\Newline (getf data-plist key)))
            (setf (getf data-plist key) (get-default-value data-plist key)) ;; for order ?
            (let ((default (get-default-value data-plist key))
                  (input "")) 
              (format t "~A: (default: ~A) " key default)
              (force-output)
              (setf input (read-line))
              (setf (getf data-plist key) (if (string= input "") default input))))))
  (dolist (key (plist-keys data-plist)) ;;
    (set-value key name (get-default-value data-plist key)))
  (save-data name))

;;; MAKE-PAGES
(defun read-template (template-name)
  (read-template-form-file (get-template-path template-name)))

(defun make-page (name &key (template-name nil))
  ;; TODO
  (unless  (probe-file (get-text-path name)) (return-from make-page nil))
  (unless (and (stringp template-name) 
               (probe-file (get-template-path template-name)))
    ;; if template-file specified by template-name dosen't exist, use default template file for name
    (setf template-name (find-template name))) 
  (write-file (get-page-path name) (convert name (read-template template-name)))
  (write-time name))

(defun auto-update (name)
  (let ((parent (get-value "PARENT" name))
        (prev (get-value "PREV" name)))
    (when (and parent (not (string= parent "")) (probe-file (get-text-path parent)))
      (pushnew-value "CHILD" parent name t)
      (make-page parent))
    (when (and prev (not (string= prev "")) (probe-file (get-text-path prev)))
      (set-value "NEXT" prev name t)
      (make-page prev))))
