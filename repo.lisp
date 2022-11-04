;;  repo  -  common interface for version control systems
;;  Copyright 2016-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :common-lisp-user)

(defpackage :repo
  (:use :common-lisp)
  (:export #:boot
           #:clear-repos
           #:find-repo
           #:find-repo-by-package
           #:git
           #:github
           #:install
           #:manifest
           #:*manifest*
           #:repo
           #:*repo-dir*
           #:*repos*
           #:svn
           #:update))

(defpackage :repo-user
  (:use :common-lisp :repo))

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

(defun string-split (s x)
  (let ((p (search s x)))
    (if p
        (cons (subseq x 0 p)
              (string-split s (subseq x (+ (length s) p))))
        (cons x nil))))

(defun first-line (x)
  (let ((newline (position #\Newline x)))
    (if newline
        (subseq x 0 newline)
        x)))

(defun dirname (x)
  (let ((slash (position #\/ x :from-end t
                         :end (or (string-ends-with "/" x) (length x)))))
    (cond ((null slash) "")
          ((= 0 slash) "/")
          (t (subseq x 0 slash)))))

(defun basename (x)
  (let* ((end (or (string-ends-with "/" x) (length x)))
         (slash (position #\/ x :from-end t :end end)))
    (cond ((null slash) (subseq x 0 end))
          (t (subseq x (1+ slash) end)))))

(defun probe-dir (x)
  #+clisp (ext:probe-directory (format nil "~A/" x))
  #-clisp (probe-file (format nil "~A/" x)))

(defun str (&rest parts)
  (labels ((to-str (x)
             (typecase x
               (string x)
               (null "")
               (cons (apply 'str x))
               (pathname (namestring x))
               (t (prin1-to-string x)))))
    (apply 'concatenate 'string (mapcar #'to-str parts))))

(defun kw (x)
  (intern (string-upcase x) (find-package :keyword)))

(defun translate-home (x)
  (if (string-starts-with "~/" x)
      (str (user-homedir-pathname) (subseq x 2))
      x))

;;  shell commands

;;  TODO: run-program for all lisp implementations
#+sbcl
(defun run-program (cmd &rest args)
  (format t "~&$ ~S~{ ~S~}~%" cmd args)
  (let* ((out (make-string-output-stream))
         (err (make-string-output-stream))
         (process (sb-ext:run-program cmd args
                                      :output out
                                      :error err
                                      :external-format :utf-8))
         (exit-code (sb-ext:process-exit-code process)))
    (close out)
    (close err)
    (let ((out (get-output-stream-string out))
          (err (get-output-stream-string err)))
      (format t "~&~S~&" out)
      (format t "~&~S~&" err)
      (unless (= 0 exit-code)
        (with-simple-restart (continue "Ignore command error")
          (error "~&$ ~S~{ ~S~}~%~S" cmd args err)))
      (values out err exit-code))))

#+clisp
(defun run-program (cmd &rest args)
  (format t "~&$ ~A~{ ~A~}~%" cmd args)
  (let* ((buf (make-array '(4096) :element-type 'character))
         (stream (ext:run-program cmd :arguments args
                                  :output :stream :wait t))
         (len (read-sequence buf stream))
         (out (subseq buf 0 len)))
    (format t "~&~A~&" out)
    (values out "" 0)))

#-windows
(defun sh (&rest parts)
  (run-program "/bin/sh" "-c" (str parts)))

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

(defvar *repos* ())

(defclass repo ()
  ((dir :initarg :dir
        :reader repo-dir
        :type string)
   (name :initarg :name
         :reader repo-name
         :type string)
   (head :initarg :head
         :type string)
   (uri :initarg :uri
        :reader repo-uri
        :type string)
   (url :initarg :url
        :reader repo-url
        :type string)
   (local-dir :initarg :local-dir
              :reader repo-local-dir
              :type string)
   (packages :initarg :packages
             :reader repo-packages
             :type list)))

(defgeneric repo-asd (repo &optional package))
(defgeneric repo-dir/name (repo))
(defgeneric repo-head (repo))
(defgeneric repo-head-default (repo))
(defgeneric repo-package-p (x repo))

(defmethod print-object ((obj repo) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (dir name uri local-dir packages) obj
      (format stream "~A/~A ~S ~S ~S" dir name uri local-dir
              (when (slot-boundp obj 'packages)
                packages)))))

(defmethod initialize-instance :after ((repo repo) &rest initargs)
  (declare (ignore initargs))
  (with-slots (dir name packages) repo
    (setf (slot-value repo 'local-dir)
          (format nil "~A/~A/~A" *repo-dir* dir name))
    (unless (slot-boundp repo 'packages)
      (setf packages (list name)))))

(defmethod repo-asd ((repo repo) &optional
                                   (package (first (repo-packages repo))))
  (namestring
   (first
    (directory
     (str (translate-home (repo-local-dir repo)) "/**/"
          (string-downcase package) ".asd")))))

(defmethod repo-dir/name ((repo repo))
  (str (repo-dir repo) "/" (repo-name repo)))

(defmethod repo-head ((repo repo))
  (if (slot-boundp repo 'head)
      (slot-value repo 'head)
      (repo-head-default repo)))

(defmethod repo-package-p (x repo)
  (find x (repo-packages repo) :test #'string-equal))

(defun repo-by-url (url)
  (find url *repos* :key #'repo-url :test #'string=))

(defun repo-by-uri (uri)
  (find uri *repos* :key #'repo-uri :test #'string=))

;;  git command

(defvar *git*
  #+unix (or (probe-file "/usr/bin/git")
             (probe-file "/usr/local/bin/git")
             (first-line (sh "which git"))))

(defun $git (&rest args)
  (apply 'run-program *git* args))

;;  git repository class

(defclass git-repo (repo) ())

(defgeneric $git-checkout (repo))
(defgeneric $git-clone (repo))
(defgeneric $git-fetch (repo))
(defgeneric $git-pull (repo))

(defmethod $git-checkout ((repo git-repo))
  (let* ((local (repo-local-dir repo))
         (head (repo-head repo))
         (str-head (str head))
         (args `("-C" ,(translate-home local) "checkout"
                      ,@(unless (= 0 (length str-head))
                          '(str-head)))))
    (apply #'$git args)
    nil))

(defmethod $git-clone ((repo git-repo))
  (let ((local (repo-local-dir repo))
        (url (repo-url repo)))
    (when (probe-dir local)
      (error "git clone: not overwriting existing local directory~&~S" local))
    (let ((parent (dirname local)))
      (ensure-directories-exist (str parent "/") :verbose t)
      ($git "-C" (translate-home parent) "clone" url)
      nil)))

(defmethod $git-fetch ((repo git-repo))
  (let ((local (repo-local-dir repo)))
    ($git "-C" (translate-home local) "fetch")
    nil))

(defmethod $git-pull ((repo git-repo))
  (let ((local (repo-local-dir repo)))
    ($git "-C" (translate-home local) "pull")
    nil))

(defmethod install ((repo git-repo))
  (let ((local (repo-local-dir repo)))
    (unless (probe-dir local)
      ($git-clone repo))
    (let ((asd (repo-asd repo)))
      (when asd
        (asdf::load-asd asd)))))

(defmethod repo-head-default ((repo git-repo))
  "master")

(defmethod update ((repo git-repo))
  (when (probe-dir (repo-local-dir repo))
    ($git-pull repo)))

(defun git-repo-uri-handler (uri)
  (let ((uri (first (string-split "#" uri))))
    (let ((start (or (string-starts-with "git://" uri)
                     (string-starts-with "http://" uri)
                     (string-starts-with "https://" uri))))
      (when start
        (let* ((dot (search ".git" uri :from-end t))
               (slash (position #\/ uri :end dot :from-end t))
               (slash2 (position #\/ uri :end slash :from-end t))
               (dir (subseq uri (1+ slash2) slash))
               (name (subseq uri (1+ slash) dot)))
          `(git-repo :dir ,dir :name ,name
                     :uri ,uri
                     :url ,uri))))))

(defun git (url &rest initargs)
  (or (repo-by-url url)
      (let ((repo (apply #'make-instance
                         (append (git-repo-uri-handler url) initargs))))
        (push repo *repos*)
        repo)))

;;  github repository class

(defclass github-repo (git-repo) ())

(defmethod print-object ((obj github-repo) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (dir name local-dir packages) obj
      (format stream "~A/~A ~S ~S" dir name local-dir packages))))

(defun github-uri (user name &optional head package)
  (str "github:" user "/" name
       (when head "?") head
       (when package "#") package))

(defun github-url (user name)
  (str "https://github.com/" user "/" name ".git"))

(defun github-repo-uri-handler (uri)
  (let ((uri (first (string-split "#" uri))))
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
                        :url ,(github-url user name)))))))

(defun github (user name &rest initargs)
  (let ((uri (github-uri user name)))
    (or (repo-by-uri uri)
        (let ((repo (apply #'make-instance 'github-repo
                           :dir user :name name
                           :uri uri
                           :url (github-url user name)
                           initargs)))
          (push repo *repos*)
          repo))))

;;  subversion command

(defvar *svn*
  #+unix (or (probe-file "/usr/bin/svn")
             (probe-file "/usr/local/bin/svn")
             (first-line (ignore-errors (sh "which svn")))))

(defun $svn (&rest args)
  (apply 'run-program *svn* args))

;;  subversion repository class

(defclass svn-repo (repo) ())

(defgeneric $svn-checkout (repo))
(defgeneric $svn-update (repo))

(defmethod $svn-checkout ((repo svn-repo))
  (let ((local (repo-local-dir repo))
        (url (repo-url repo)))
    (when (probe-dir local)
      (error "svn checkout: not overwriting existing local directory~&~S" local))
    (let ((parent (dirname local)))
      (ensure-directories-exist (str parent "/") :verbose t)
      ($svn "co" url (translate-home local))
      nil)))

(defmethod $svn-update ((repo svn-repo))
  (let ((local (repo-local-dir repo)))
    ($svn "up" (translate-home local))
    nil))

(defmethod install ((repo svn-repo))
  (let ((local (repo-local-dir repo)))
    (unless (probe-dir local)
      ($svn-checkout repo))
    (asdf::load-asd (repo-asd repo))))

(defmethod update ((repo svn-repo))
  (when (probe-dir (repo-local-dir repo))
    ($svn-update repo)))

(defun svn (url dir/name &rest initargs)
  (or (repo-by-url url)
      (let ((repo (apply #'make-instance 'svn-repo
                         :dir (dirname dir/name)
                         :name (basename dir/name)
                         :url url :uri url
                         :head (basename url)
                         initargs)))
        (push repo *repos*)
        repo)))

;;  repo uri handler

(defparameter *repo-uri-handlers*
  '(github-repo-uri-handler
    git-repo-uri-handler))

(defun clear-repos ()
  (setf *repos* nil))

(defun find-repo (uri)
  (let ((uri (string uri)))
    (or (find uri *repos* :key 'repo-uri :test 'string=)
        (if (position #\/ uri)
            (find uri *repos* :key 'repo-dir/name :test 'string-equal)
            (find uri *repos* :key 'repo-name :test 'string-equal)))))

(defun find-repo-by-package (x)
  (find x *repos* :test #'repo-package-p))

(defun uri-fragment (x)
  (second (string-split "#" x)))

(defun repo (uri)
  "Factory function for repository classes using *REPO-URI-HANDLERS*."
  (when (symbolp uri)
    (setq uri (symbol-name uri)))
  (destructuring-bind (uri &rest packages) (string-split " " uri)
    (or (find-repo uri)
        (when (stringp uri)
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
                       (initargs (plist-merge initargs
                                              `(:packages ,(or packages
                                                               `(,kw))))))
                  (or (find-repo uri)
                      (let ((repo (apply 'make-instance class initargs)))
                        (push repo *repos*)
                        repo))))))))))

(defun repo-or-die (x)
  (or (repo x)
      (error "unknown repository : ~S" x)))

(defmethod $git-clone ((uri string))
  ($git-clone (repo-or-die uri)))

(defmethod $git-pull ((uri string))
  ($git-pull (repo-or-die uri)))

;;  repos list

(defmethod install ((repos cons))
  (map nil 'install repos))

(defmethod update ((repos cons))
  (map nil 'update repos))

;;  manifest

(defvar *manifest*)

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

(defmethod print-object ((obj manifest) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~S ~A repos"
            (manifest-file obj)
            (length (manifest-repos obj)))))

(defmethod manifest-file ((manifest manifest))
  (str (manifest-dir manifest) "/repo.manifest"))

(defun manifest-from-file (pathname)
  (let ((*repos* nil)
        (write-date (file-write-date pathname)))
    (load pathname)
    (make-instance 'manifest
                   :write-date write-date
                   :dir (dirname pathname)
                   :repos *repos*)))

(defmethod reload-manifest ((manifest manifest))
  (let* ((pathname (manifest-file manifest))
         (*repos* nil)
         (write-date (file-write-date pathname)))
    (load pathname)
    (setf (manifest-write-date manifest) write-date
          (manifest-repos manifest) *repos*))
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
  (when *manifest*
    (maybe-reload-manifest *manifest*))
  (setq *repos* (manifest-repos *manifest*))
  (if (manifest-file-p x)
      (install (manifest-or-die x))
      (install (repo-or-die x))))

(defmethod install ((x null))
  nil)

(defmethod install ((x symbol))
  (when *manifest*
    (maybe-reload-manifest *manifest*))
  (setq *repos* (manifest-repos *manifest*))
  (install (repo-or-die x)))

(defmethod update ((x string))
  (when *manifest*
    (maybe-reload-manifest *manifest*))
  (setq *repos* (manifest-repos *manifest*))
  (if (manifest-file-p x)
      (update (manifest-or-die x))
      (update (repo-or-die x))))

(defmethod update ((x null))
  nil)

(defmethod update ((x symbol))
  (when *manifest*
    (maybe-reload-manifest *manifest*))
  (setq *repos* (manifest-repos *manifest*))
  (update (repo-or-die x)))

;;  system-definition

(defun sysdef (x)
  (when *manifest*
    (maybe-reload-manifest *manifest*))
  (setq *repos* (manifest-repos *manifest*))
  (let ((repo (or (find-repo-by-package x)
                  (repo x))))
    (when repo
      (install repo)
      (pathname (repo-asd repo x)))))

;;  setup

(defun boot ()
  (let ((manifest-file (str *repo-dir* "/repo.manifest")))
    (when (probe-file manifest-file)
      (setq *manifest* (manifest manifest-file))
      (setq *repos* (manifest-repos *manifest*))
      (when (find-package :asdf)
        (pushnew 'sysdef asdf:*system-definition-search-functions*)))))
