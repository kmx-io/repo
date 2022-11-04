;; repo - common interface for version control systems
;; Copyright 2016-2022 kmx.io <contact@kmx.io>
;;
;; Permission is hereby granted to use this software granted
;; the above copyright notice and this permission paragraph
;; are included in all copies and substantial portions of this
;; software.
;;
;; THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
;; PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
;; AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
;; THIS SOFTWARE.

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
