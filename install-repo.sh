#!/bin/sh
set -e

REPO_DIR=$HOME/common-lisp
REPO_DIR=/tmp/common-lisp

GITHUB='https://github.com/'

# detect gnu make

if which gmake; then
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

maybe_clone "${GITHUB}" 'kmx-io' 'repo'
if ! [ -f "${REPO_DIR}/repo.manifest" ]; then
    ( cd "${REPO_DIR}" && ln -s kmx-io/repo/repo.manifest; )
fi

# Configure SBCL

{
    echo "(load \"${REPO_DIR}/fare/asdf/build/asdf\")"
    echo "(load \"${REPO_DIR}/kmx-io/repo/repo\")"
    echo "(repo:boot)"
} >> ~/.sbclrc
