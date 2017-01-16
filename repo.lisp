;;
;;  repo  -  common interface for version control systems
;;
;;  Copyright 2016-2017 Thomas de Grivel <thomas@lowh.net>
;;

(in-package :common-lisp-user)

(defpackage :repo
  (:use :common-lisp)
  (:export #:repo
	   #:manifest
	   #:install
	   #:update
	   #:*repo-dir*
	   #:*repos*
	   #:clear-repos))

(in-package :repo)

(defvar *repo-dir* "~/common-lisp")

;;  string functions

(defvar *spaces* (coerce '(#\Space #\Tab) 'string))

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

(defun kw (x)
  (intern (string-upcase x) (find-package :keyword)))

;;  shell commands

;;  TODO: run-program for all lisp implementations
#+sbcl
(defun run-program (cmd &rest args)
  (let ((out (make-string-output-stream))
	(err (make-string-output-stream)))
    (let* ((process (sb-ext:run-program cmd args
					:output out
					:error err
					:external-format :utf-8))
	   (exit-code (sb-ext:process-exit-code process)))
      (close out)
      (close err)
      (let ((out (get-output-stream-string out))
	    (err (get-output-stream-string err)))
	(values exit-code out err)))))

#-windows
(defun sh (&rest parts)
  (let ((cmd (str parts)))
    (format t "~&$ ~A~%" cmd)
    (multiple-value-bind (exit-code out err) (run-program "/bin/sh" "-c" cmd)
      (format t "~&~A~&" out)
      (format t "~&~A~&" err)
      (unless (= 0 exit-code)
	(with-simple-restart (continue "Ignore shell error")
	  (error "$ ~A~%~A" cmd err)))
      (values out err exit-code))))

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

(defun sh-quote-dir (x)
  (let ((home (string-starts-with "~/" x)))
    (if home
	(str "~/" (sh-quote (subseq x home)))
	(sh-quote x))))

;;  property list functions

(defun plist-merge (to add &rest more-lists)
  (cond
    ((endp add) (if (endp more-lists)
		    to
		    (plist-merge to (first more-lists) (rest more-lists))))
    ((endp (rest add)) (error "Incomplete property list"))
    (t (setf (getf to (first add)) (first (rest add)))
       (plist-merge to (rest (rest add))))))

;;  generic functions

(defgeneric install (obj))
(defgeneric update (obj))

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

(defgeneric repo-dir/name (repo))

(defmethod repo-dir/name ((repo repo))
  (str (repo-dir repo) "/" (repo-name repo)))

(defmethod print-object ((obj repo) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (dir name uri local-dir packages) obj
      (format stream "~A/~A ~S ~S ~S" dir name uri local-dir packages))))

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
      (ensure-directories-exist (str parent "/") :verbose t)
      (sh "cd " (sh-quote-dir parent) " && git clone " (sh-quote url))
      nil)))

(defmethod install ((repo git-repo))
  (let ((local (repo-local-dir repo)))
    (if (probe-dir local)
	(git-pull repo)
	(git-clone repo))))

(defmethod git-pull ((repo git-repo))
  (let ((local (repo-local-dir repo)))
    (unless (probe-dir local)
      (error "git pull: ~S: no such file or directory" local))
    (sh "cd " (sh-quote-dir local) " && git pull")
    nil))

(defmethod update ((repo git-repo))
  (when (probe-dir (repo-local-dir repo))
    (git-pull repo)))

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

(defun github-repo-uri-handler (uri)
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
  '(github-repo-uri-handler))

(defvar *repos* ())

(defun clear-repos ()
  (setf *repos* nil))

(defun find-repo (uri)
  (or (find uri *repos* :key 'repo-uri :test 'string=)
      (if (position #\/ uri)
	  (find uri *repos* :key 'repo-dir/name :test 'string=)
	  (find uri *repos* :key 'repo-name :test 'string=))))

(defun repo (uri &rest args &key packages &allow-other-keys)
  "Factory function for repository classes using *REPO-URI-HANDLERS*."
  (labels ((do-handlers (handlers)
	     (when handlers
	       (or (funcall (first handlers) uri)
		   (do-handlers (rest handlers))))))
    (let ((spec (do-handlers *repo-uri-handlers*)))
      (when spec
	(let* ((class (first spec))
	       (initargs (rest spec))
	       (uri (getf initargs :uri))
	       (kw (kw (getf initargs :name)))
	       (initargs (plist-merge initargs args
				      `(:packages ,(or packages `(,kw))))))
	  (or (find-repo uri)
	      (let ((repo (apply 'make-instance class initargs)))
		(push repo *repos*)
		repo)))))))

(defun repo-or-die (x)
  (or (repo x)
      (error "unknown repository descriptor : ~S" x)))

(defmethod git-clone ((uri string))
  (git-clone (repo-or-die uri)))

(defmethod git-pull ((uri string))
  (git-pull (repo-or-die uri)))

;;  repos list

(defun repos-from-stream (stream)
  (loop for line = (read-line stream nil)
     while line
     for spec = (string-trim *spaces* line)
     unless (string-starts-with "#" spec)
     collect (repo-or-die spec)))

(defun repos-from-file (pathname)
  (with-open-file (in pathname :element-type 'character
		      :external-format :utf-8)
    (values (repos-from-stream in) (file-write-date in))))

(defmethod install ((repos cons))
  (map nil 'install repos))

(defmethod update ((repos cons))
  (map nil 'update repos))

;;  manifest

(defclass manifest ()
  ((write-date :initarg :write-date
	       :accessor manifest-write-date
	       :type rational)
   (dir :initarg :dir
	:reader manifest-dir
	:type string)
   (repos :initarg :repos
	  :accessor manifest-repos
	  :type list)))

(defgeneric manifest-file (manifest))
(defgeneric reload-manifest (manifest))
(defgeneric maybe-reload-manifest (manifest))

(defmethod manifest-file ((manifest manifest))
  (str (manifest-dir manifest) "/repo.manifest"))

(defun manifest-from-file (pathname)
  (multiple-value-bind (repos write-date)
      (repos-from-file pathname)
    (make-instance 'manifest
		   :write-date write-date
		   :dir (dirname pathname)
		   :repos repos)))

(defmethod reload-manifest ((manifest manifest))
  (multiple-value-bind (repos write-date)
      (repos-from-file (manifest-file manifest))
    (setf (manifest-write-date manifest) write-date
	  (manifest-repos manifest) repos))
  manifest)

(defmethod maybe-reload-manifest ((manifest manifest))
  (if (< (manifest-write-date manifest)
	 (file-write-date (manifest-file manifest)))
      (reload-manifest manifest)
      manifest))

(defmethod install ((manifest manifest))
  (let ((manifest (maybe-reload-manifest manifest)))
    (let ((*repo-dir* (manifest-dir manifest))
	  (*repos* (manifest-repos manifest)))
      (install *repos*))))

(defmethod update ((manifest manifest))
  (let ((manifest (maybe-reload-manifest manifest)))
    (let ((*repo-dir* (manifest-dir manifest))
	  (*repos* (manifest-repos manifest)))
      (update *repos*))))

;;  manifest uri handlers

(defun manifest-file-p (x)
  (or (string= "repo.manifest" x)
      (string-ends-with "/repo.manifest" x)))

(defun local-manifest-uri-handler (x)
  (let ((end (string-ends-with "/repo.manifest" x)))
    (when end
      (let* ((*repo-dir* (subseq x 0 end))
	     (manifest (str *repo-dir* "/repo.manifest")))
	(when (probe-file manifest)
	  (manifest-from-file manifest))))))

(defvar *manifest-uri-handlers*
  '(local-manifest-uri-handler))

(defun manifest (uri)
  "Load manifest from uri"
  (labels ((do-handlers (handlers)
	     (unless (endp handlers)
	       (or (funcall (first handlers) uri)
		   (do-handlers (rest handlers))))))
    (do-handlers *manifest-uri-handlers*)))

(defun manifest-or-die (uri)
  (or (manifest uri) (error "failed to load manifest ~S" uri)))

;;  install and update commands

(defmethod install ((x string))
  (if (manifest-file-p x)
      (install (manifest-or-die x))
      (install (repo-or-die x))))

(defmethod update ((x string))
  (if (manifest-file-p x)
      (update (manifest-or-die x))
      (update (repo-or-die x))))
