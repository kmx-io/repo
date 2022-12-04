#!/bin/sh
## Copyright 2016-2022 kmx.io <contact@kmx.io>
##
## Permission is hereby granted to use this software granted
## the above copyright notice and this permission paragraph
## are included in all copies and substantial portions of this
## software.
##
## THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
## PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
## AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
## THIS SOFTWARE.

set -e

REPO_DIR=$HOME/common-lisp

GITHUB='https://github.com/'
KMX='https://git.kmx.io/'

# detect gnu make

if which gmake > /dev/null; then
    MAKE=gmake
else
    MAKE=make
fi

# clone git repo

maybe_clone() {
    HOST="$1"
    OWNER="$2"
    NAME="$3"
    if ! [ -d "${REPO_DIR}/${OWNER}/${NAME}" ]; then
        echo "Installing ${HOST}${OWNER}/${NAME} into ${REPO_DIR}/${OWNER}/${NAME}"
        mkdir -p "${REPO_DIR}/${OWNER}"
        git -C "${REPO_DIR}/${OWNER}" clone "${HOST}${OWNER}/${NAME}"
    fi
}

# Install ASDF from Github

maybe_clone "${GITHUB}" 'fare' 'asdf'
"${MAKE}" -C "${REPO_DIR}/fare/asdf"

# Install Repo from Github

maybe_clone "${KMX}" 'kmx.io' 'repo'
if ! [ -f "${REPO_DIR}/repo-index.lisp" ]; then
    echo "Linking ${REPO_DIR}/repo-index.lisp"
    ( cd "${REPO_DIR}" && ln -s kmx.io/repo/repo-index.lisp; )
fi

# Configure SBCL

if grep -q "(load \"${REPO_DIR}/fare/asdf/build/asdf\")" ~/.sbclrc &&
   grep -q "(load \"${REPO_DIR}/kmx.io/repo/repo\")" ~/.sbclrc &&
   grep -q "(repo:boot)" ~/.sbclrc; then
    :
else
    echo Appending to ~/.sbclrc
    {
        echo "(load \"${REPO_DIR}/fare/asdf/build/asdf\")"
        echo "(load \"${REPO_DIR}/kmx.io/repo/repo\")"
        echo "(repo:boot)"
    } >> ~/.sbclrc
fi
