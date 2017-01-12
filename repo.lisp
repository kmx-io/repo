
(defpackage :repo
  (:use :common-lisp)
  (:export #:repo
	   #:repo-install
	   #:repo-update))

(in-package :repo)

;;  string functions

(defun string-starts-with (x string)
  (let ((lx (length x))
	(ls (length string)))
    (when (and (>= ls lx)
	       (string= x string :end2 lx))
      lx)))

(defun string-ends-with (x string)
  (let* ((lx (length x))
	 (ls (length string))
	 (dl (- ls lx)))
    (when (and (>= ls lx)
	       (string= x string :start2 dl))
      dl)))

(defun dirname (x)
  (let ((slash (position #\/ x :from-end t
			 :end (or (string-ends-with "/" x) (length x)))))
    (cond ((null slash) "")
	  ((= 0 slash) "/")
	  (t (subseq x 0 slash)))))

(defun probe-dir (x)
  (probe-file (format nil "~A/" x)))

(defun str (&rest parts)
  (labels ((to-str (x)
	     (typecase x
	       (string x)
	       (null "")
	       (cons (apply 'str x))
	       (t (prin1-to-string x)))))
    (apply 'concatenate 'string (mapcar #'to-str parts))))

;;  shell commands

(defun sh (&rest parts)
  (let ((cmd (str parts))
	(out (make-string-output-stream))
	(err (make-string-output-stream)))
    (format t "~&$ ~A~%" cmd)
    (let* ((process (sb-ext:run-program "/bin/sh" `("-c" ,cmd)
					:output out
					:error err
					:external-format :utf-8))
	   (exit-code (sb-ext:process-exit-code process)))
      (close out)
      (close err)
      (let ((out (get-output-stream-string out))
	    (err (get-output-stream-string err)))
	(unless (= 0 exit-code)
	  (with-simple-restart (continue "Ignore shell error")
	    (error "$ ~A~%~A" cmd err))
	  (values out err exit-code))))))

(defvar *sh-unquoted-chars*
  "+,-./0123456789:=ABCDEFGHIJKLMNOPQRSTUVWXYZ^_abcdefghijklmnopqrstuvwxyz")

(defvar *sh-quoted-chars*
  "\"$\\`")

(defun sh-need-quote (x)
  (dotimes (i (length x))
    (unless (find (char x i) *sh-unquoted-chars*)
      (return t))))

(defun sh-quote (x)
  (if (sh-need-quote x)
      (with-output-to-string (out)
	(write-char #\" out)
	(dotimes (i (length x))
	  (let ((c (char x i)))
	    (when (find c *sh-quoted-chars*)
	      (write-char #\\ out))
	    (write-char c out)))
	(write-char #\" out))
      x))
	
;;  property list functions

(defun plist-merge (to add &rest more-lists)
  (cond
    ((endp add) (if (endp more-lists)
		    to
		    (plist-merge to (first more-lists) (rest more-lists))))
    ((endp (rest add)) (error "Incomplete property list"))
    (t (setf (getf to (first add)) (first (rest add)))
       (plist-merge to (rest (rest add))))))

;;  repository base class

(defclass repo ()
  ((dir :initarg :dir
	:reader repo-dir
	:type string)
   (name :initarg :name
	 :reader repo-name
	 :type string)
   (uri :initarg :uri
	:reader repo-uri
	:type string)
   (url :initarg :url
	:reader repo-url
	:type string)
   (local-dir :initarg :local-dir
	      :reader repo-local-dir
	      :type pathname)
   (packages :initarg :packages
	     :reader repo-packages
	     :type list)))

(defgeneric repo-install (repo))
(defgeneric repo-update (repo))

(defmethod print-object ((obj repo) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (dir name uri local-dir packages) obj
      (format stream "~A/~A ~S ~S ~S" dir name uri local-dir packages))))

(defvar *repo-dir* "~/common-lisp")

(defmethod initialize-instance :after ((repo repo) &rest initargs)
  (declare (ignore initargs))
  (with-slots (dir name) repo
    (setf (slot-value repo 'local-dir)
	  (format nil "~A/~A/~A" *repo-dir* dir name))))

;;  git repository class

(defclass git-repo (repo) ())

(defgeneric git-clone (repo))
(defgeneric git-pull (repo))

(defmethod git-clone ((repo git-repo))
  (let ((local (repo-local-dir repo))
	(url (repo-url repo)))
    (when (probe-dir local)
      (error "git clone: not overwriting existing local directory~&~S" local))
    (let ((parent (dirname local)))
      (ensure-directories-exist parent :verbose t)
      (sh "cd " (sh-quote parent) " && git clone " (sh-quote url))))) 

(defmethod repo-install ((repo git-repo))
  (let ((local (repo-local-dir repo)))
    (unless (probe-dir local)
      (git-clone repo))))

(defmethod git-pull ((repo git-repo))
  (let ((local (repo-local-dir repo)))
    (unless (probe-dir local)
      (error "git pull: ~S: no such file or directory" local))
    (sh "cd " (sh-quote local) " && git pull")))

(defmethod repo-update ((repo git-repo))
  (repo-install repo)
  (git-pull repo))

;;  github repository class

(defclass github-repo (git-repo) ())

(defmethod print-object ((obj repo) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (dir name local-dir packages) obj
      (format stream "~A/~A ~S ~S" dir name local-dir packages))))

(defun github-uri (user name)
  (format nil "github:~A/~A" user name))

(defun github-url (user name)
  (format nil "http://github.com/~A/~A.git" user name))

(defun github-uri-handler (uri)
  (let ((start (or (string-starts-with "github:" uri)
		   (string-starts-with "git://github.com/" uri)
		   (string-starts-with "http://github.com/" uri)
		   (string-starts-with "https://github.com/" uri))))
    (when start
      (let* ((slash (or (position #\/ uri :start start)
			(error "Invalid repo uri ~S" uri)))
	     (dot (or (string-ends-with ".git/" uri)
		      (string-ends-with ".git" uri)
		      (string-ends-with "/" uri)))
	     (user (subseq uri start slash))
	     (name (subseq uri (1+ slash) dot)))
	`(github-repo :dir ,user :name ,name
		      :uri ,(github-uri user name)
		      :url ,(github-url user name))))))

;;  repo uri handler

(defvar *repo-uri-handlers*
  '(github-uri-handler))

(defun repo (url &rest args &key packages &allow-other-keys)
  "Factory function for repository classes using *REPO-URI-HANDLERS*."
  (labels ((do-handlers (handlers)
	     (when handlers
	       (or (funcall (first handlers) url)
		   (do-handlers (rest handlers))))))
    (let ((spec (do-handlers *repo-uri-handlers*)))
      (when spec
	(let* ((class (first spec))
	       (initargs (rest spec))
	       (kw (intern (string-upcase (getf initargs :name)) :keyword))
	       (initargs (plist-merge initargs args
				      (list :packages
					    (or packages (list kw))))))
	  (apply 'make-instance class initargs))))))

(defmethod repo-install ((uri string))
  (repo-install (repo uri)))

(defmethod repo-update ((uri string))
  (repo-update (repo uri)))

(defmethod git-clone ((uri string))
  (git-clone (repo uri)))

(defmethod git-pull ((uri string))
  (git-pull (repo uri)))

(repo "github:thodg/repo-install")
