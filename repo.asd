;;
;;  repo - source repository utilities
;;  Copyright 2016-2017 Thomas de Grivel <thomas@lowh.net>
;;

(defpackage :repo.system
  (:use :cl :asdf))

(in-package :repo.system)

(defsystem :repo
  :name "repo"
  :author "Thomas de Grivel <thomas@lowh.net>"
  :version "0.1"
  :description "source repository utilities"
  :depends-on ()
  :components
  ((:file "repo")))
