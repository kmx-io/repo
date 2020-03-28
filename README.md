# repo

Common interface for version control systems.

Repo allows you to use source repositories directly as ASDF-installable
packages and keep them synced with upstream for development purposes.

Each repo is installed in a subdirectory.
Github repositories are installed in the user subdirectory.

## Quickstart

``` SH
curl -O https://github.com/common-lisp-repo/repo/blob/master/install-repo.sh
sh install-repo.sh
```

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
  mkdir -p ~/common-lisp/common-lisp-repo
  cd ~/common-lisp/common-lisp-repo
  git clone https://github.com/common-lisp-repo/repo.git
```

In your Common Lisp implementation startup file :

``` Common-Lisp
  (load "~/common-lisp/fare/asdf/build/asdf")
  (load "~/common-lisp/common-lisp-repo/repo/repo")
  (repo:boot)
```

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
  (repo:repo "github:thodg/repo")         ;; Define repository by URI

  (repo:repo "thodg/repo")                ;; Find repository by dir/name
  (repo:repo :repo)                       ;; Find repository by name

  (setf repo:*repo-dir* "/tmp/repo-test") ;; Change installation directory

  (repo:install "github:thodg/repo")      ;; Install repository by URI
  (repo:install "thodg/repo")             ;; Install repository by dir/name
  (repo:install :repo)                    ;; Install repository by name

  (repo:update "github:thodg/repo")       ;; Update repository by URI
  (repo:update "thodg/repo")              ;; Update repository by dir/name
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
