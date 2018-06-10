;;
;;  repo  -  common interface for version control systems
;;
;;  Copyright 2016-2018 Thomas de Grivel <thoxdg@gmail.com>
;;

(defpackage :repo.system
  (:use :cl :asdf))

(in-package :repo.system)

(defsystem :repo
  :name "repo"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.2"
  :description "common interface for version control systems"
  :depends-on ()
  :components
  ((:file "repo")))
