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

(in-package :repo-user)

(git "http://git.kpe.io/cl-base64.git")
(git "http://git.kpe.io/puri.git")
(git "https://gitlab.common-lisp.net/alexandria/alexandria.git")
(git "https://gitlab.common-lisp.net/cl-irregsexp/cl-irregsexp.git")
(git "https://gitlab.common-lisp.net/cl-smtp/cl-smtp.git")
(git "https://gitlab.common-lisp.net/cl-utilities/cl-utilities.git")
(git "https://gitlab.common-lisp.net/metabang-bind/metabang-bind.git")
(git "https://gitlab.common-lisp.net/rfc2388/rfc2388.git")

(github "3b" "3bmd")
(github "AccelerationNet" "cl-inflector")
(github "BnMcGn" "html-entities")
(github "KDr2" "cl-fastcgi")
(github "KDr2" "sb-fastcgi")
(github "adamczykm" "iterate")
(github "ahefner" "shuffletron")
(github "binghe" "portable-threads")
(github "cffi" "cffi")
(github "chaitanyagupta" "chronicity")
(github "cl-babel" "babel")
(github "cl-plus-ssl" "cl-plus-ssl" :packages '("cl+ssl"))
(github "cosmos72" "stmx")
(github "didierverna" "clon")
(github "didierverna" "declt")
(github "didierverna" "tfm")
(github "diogoalexandrefranco" "cl-strings")
(github "dlowe-net" "local-time")
(github "drdo" "do-urlencode")
(github "e-user" "cl-heredoc")
(github "edicl" "chunga")
(github "edicl" "cl-fad")
(github "edicl" "cl-interpol")
(github "edicl" "cl-ppcre")
(github "edicl" "cl-unicode")
(github "edicl" "drakma")
(github "edicl" "flexi-streams")
(github "edicl" "hunchentoot")
(github "eugeneia" "cl-qprint")
(github "fare" "asdf")
(github "filonenko-mikhail" "cl-portaudio")
(github "froydnj" "chipz")
(github "fukamachi" "fast-http")
(github "fukamachi" "proc-parse")
(github "fukamachi" "quri")
(github "fukamachi" "smart-buffer")
(github "fukamachi" "trivial-utf-8")
(github "fukamachi" "xsubseq")
(github "gwkkwg" "trivial-backtrace")
(github "hankhero" "cl-json")
(github "jdz" "rfc2388")
(github "jech" "cl-yacc")
(github "keithj" "deoxybyte-unix")
(github "lmj" "global-vars")
(github "marijnh" "parse-js")
(github "melisgl" "named-readtables")
(github "mishoo" "cl-uglify-js")
(github "nallen05" "rw-ut")
(github "nallen05" "trivial-email-utf-8")
(github "next-browser" "next")
(github "nightfly19" "cl-arrows")
(github "nikodemus" "esrap")
(github "orthecreedence" "blackbird")
(github "orthecreedence" "cl-async" :packages '("cl-async" "cl-async-repl" "cl-async-ssl"))
(github "orthecreedence" "cl-libuv")
(github "orthecreedence" "vom")
(github "orthecreedence" "wookie")
(github "pcostanza" "closer-mop")
(github "phmarek" "yason")
(github "phoe" "safe-read")
(github "pmai" "md5")
(github "robert-strandh" "Acclimation")
(github "robert-strandh" "Awele")
(github "robert-strandh" "CLIM-demo-adventure")
(github "robert-strandh" "CLIMatis")
(github "robert-strandh" "Claire")
(github "robert-strandh" "Claret")
(github "robert-strandh" "Classeur")
(github "robert-strandh" "Climacs")
(github "robert-strandh" "Climed")
(github "robert-strandh" "Cloak")
(github "robert-strandh" "Clobber")
(github "robert-strandh" "Clordane")
(github "robert-strandh" "Cluffer-Emacs-compatibility")
(github "robert-strandh" "Clump")
(github "robert-strandh" "Cluster")
(github "robert-strandh" "Compta")
(github "robert-strandh" "Concrete-Syntax-Tree")
(github "robert-strandh" "Ducling")
(github "robert-strandh" "Eclector")
(github "robert-strandh" "Enamel")
(github "robert-strandh" "First-Climacs")
(github "robert-strandh" "Flexichain")
(github "robert-strandh" "GF-font-viewer")
(github "robert-strandh" "Gsharp")
(github "robert-strandh" "Incremental-reader")
(github "robert-strandh" "McCLIM")
(github "robert-strandh" "Nomenclatura")
(github "robert-strandh" "SICL")
(github "robert-strandh" "Second-Climacs")
(github "robert-strandh" "Spell")
(github "robert-strandh" "Stealth-mixin")
(github "robert-strandh" "Subsequence")
(github "robert-strandh" "Sudoku")
(github "robert-strandh" "Text-annotation")
(github "robert-strandh" "Trans-Clime")
(github "robert-strandh" "USE-finder")
(github "robert-strandh" "dpANS-parser")
(github "rpav" "c2ffi")
(github "rpav" "cl-autowrap")
(github "rpav" "fast-io")
(github "sellout" "external-program")
(github "sharplispers" "ironclad")
(github "sharplispers" "nibbles")
(github "sharplispers" "parse-number")
(github "sharplispers" "split-sequence")
(github "sionescu" "bordeaux-threads")
(github "sionescu" "static-vectors")
(github "slime" "slime")
(github "thephoeron" "let-over-lambda")
(github "tpapp" "ffa")
(github "trivial-features" "trivial-features")
(github "trivial-garbage" "trivial-garbage")
(github "trivial-gray-streams" "trivial-gray-streams")
(github "usocket" "usocket")
(github "vii" "teepeedee2")
(github "vseloved" "cl-redis")
(github "vseloved" "rutils")
(github "zkat" "chanl")

(kmx "RailsOnLisp" "bootstrap")
(kmx "RailsOnLisp" "bordeaux-queue")
(kmx "RailsOnLisp" "bordeaux-set")
(kmx "RailsOnLisp" "can")
(kmx "RailsOnLisp" "font-awesome")
(kmx "RailsOnLisp" "gravatar")
(kmx "RailsOnLisp" "rol-assets")
(kmx "RailsOnLisp" "rol-files")
(kmx "RailsOnLisp" "rol-log")
(kmx "RailsOnLisp" "rol-server")
(kmx "RailsOnLisp" "rol-template")
(kmx "RailsOnLisp" "rol-uri")
(kmx "RailsOnLisp" "thot")
(kmx "cffi-posix" "cffi-dirent")
(kmx "cffi-posix" "cffi-epoll")
(kmx "cffi-posix" "cffi-errno")
(kmx "cffi-posix" "cffi-fcntl")
(kmx "cffi-posix" "cffi-socket")
(kmx "cffi-posix" "cffi-stat")
(kmx "cffi-posix" "cffi-unistd")
(kmx "cl-remap" "remap")
(kmx "cl-remap" "uiop-remap")
(kmx "cl-remap" "unistd-remap")
(kmx "cl-stream" "babel-stream")
(kmx "cl-stream" "cl-stream")
(kmx "cl-stream" "matcher-stream")
(kmx "cl-stream" "parser-stream")
(kmx "cl-stream" "token-stream")
(kmx "cl-stream" "unistd-stdio")
(kmx "cl-stream" "unistd-stream")
(kmx "facts-db" "cl-facts")
(kmx "facts-db" "cl-lessp")
(kmx "facts-db" "cl-rollback")
(kmx "idl" "excel-fun.git")
(kmx "idl" "idl-calc.git")
(kmx "kmx.io" "cl-debug")
(kmx "kmx.io" "repo")
(kmx "lowh" "exec-js")
(kmx "thodg" "cffi-portaudio")
(kmx "thodg" "cfg")
(kmx "thodg" "cl-github-v3")
(kmx "thodg" "cl-github-v3")
(kmx "thodg" "cl-unix-cybernetics")
(kmx "thodg" "css-lexer")
(kmx "thodg" "css-parser")
(kmx "thodg" "fb")
(kmx "thodg" "less-lexer")
(kmx "thodg" "less-parser")
(kmx "thodg" "positional")
(kmx "thodg" "random-sequence")
(kmx "thodg" "re.git")
(kmx "thodg" "str")
(kmx "thodg" "str.git")
