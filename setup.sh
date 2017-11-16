#!/bin/bash

script_dir() {
        if [ -z "${SCRIPT_DIR}" ]; then
                # even resolves symlinks, see
                # http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
                    local SOURCE="${BASH_SOURCE[0]}"
        while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
                SCRIPT_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    fi
            echo "${SCRIPT_DIR}"
}

CODE_HOME=$(script_dir)

setup_file_link() {
    local source="$1"
    local destination="$2"
    if [ ! -e "${destination}" ]; then
        ln -s "${source}" "${destination}" &&  echo "Linked file ${source} to ${destination}"
    else
        echo "ERROR: File ${destination} already exists"
    fi
}

setup_file_link "${CODE_HOME}/zshrc" "${HOME}/.zshrc"
setup_file_link "${CODE_HOME}/emacs" "${HOME}/.emacs"
mkdir -p "${HOME}/.lein"
setup_file_link "${CODE_HOME}/lein-profiles.clj" "${HOME}/.lein/profiles.clj"
