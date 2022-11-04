# repo 0.2

Copyright 2016-2022 kmx.io <contact@kmx.io>

Permission is hereby granted to use this software granted
the above copyright notice and this permission paragraph
are included in all copies and substantial portions of this
software.

THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
THIS SOFTWARE.


## Description

Common interface for version control systems.

Repo allows you to use source repositories directly as ASDF-installable
packages and keep them synced with upstream for development purposes.

Each repo is installed in a subdirectory.
Github repositories are installed in the user subdirectory.


## Quickstart

``` SH
  ftp https://git.kmx.io/kmx.io/repo/_blob/master/install-repo.sh
  sh install-repo.sh
```

``` Common-Lisp
  ;; install Thot
  (repo:install :thot)

  ;; update ASDF
  (repo:update :asdf)
```


## Manifest

https://git.kmx.io/kmx.io/repo/_blob/master/repo.manifest


## Installation

This is what the `install-repo.sh` script does :

Install ASDF from git :

``` SH
  mkdir -p ~/common-lisp/fare
  cd ~/common-lisp/fare
  git clone https://github.com/fare/asdf.git
  cd asdf
  make
```

Install REPO from git :

``` SH
  mkdir -p ~/common-lisp/kmx.io
  cd ~/common-lisp/kmx.io
  git clone https://git.kmx.io/kmx.io/repo.git
  cd ~/common-lisp
  ln -s kmx.io/repo/repo.manifest
```

In your Common Lisp implementation startup file :

``` Common-Lisp
  (load "~/common-lisp/fare/asdf/build/asdf")
  (load "~/common-lisp/kmx.io/repo/repo")
  (repo:boot)
```

## Usage

Repo integrates with ASDF :

``` Common-Lisp
  (asdf:load-system :thot)
```

To update all repositories :

``` Common-Lisp
  (repo:update repo:*manifest*)
```

Other functions :

``` Common-Lisp
  (repo:repo "github:kmx-io/repo")        ;; Define repository by URI

  (repo:repo "thodg/repo")                ;; Find repository by dir/name
  (repo:repo :repo)                       ;; Find repository by name

  (setf repo:*repo-dir* "/tmp/repo-test") ;; Change installation directory

  (repo:install "github:kmx-io/repo")     ;; Install repository by URI
  (repo:install "kmx.io/repo")            ;; Install repository by dir/name
  (repo:install :repo)                    ;; Install repository by name

  (repo:update "github:kmx-io/repo")      ;; Update repository by URI
  (repo:update "kmx-io/repo")             ;; Update repository by dir/name
  (repo:update :repo)                     ;; Update repository by name

  repo:*repos*                            ;; List of defined repositories

  (repo:clear-repos)                      ;; Clear all definitions
```


## Version informations

This version only supports git repositories and relies on /bin/sh.
Next releases will support other VCS / systems.

SBCL and CLISP are supported.

## TODO

*   use UIOP:RUN-PROGRAM
*   git tags and branches
*   CVS
*   subversion
*   bzr
*   darcs
*   mercurial
