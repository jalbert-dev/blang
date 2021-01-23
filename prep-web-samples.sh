#!/bin/bash

# I am not good with bash, cut me some slack

DST_WWWROOT="${1}"
DST_FOLDER_NAME="generated"
DST_FOLDER_PATH="${DST_WWWROOT}/${DST_FOLDER_NAME}"
REPO_ROOT="$(pwd)"

[[ -z ${DST_WWWROOT} ]] && { echo "Need a destination wwwroot! (outputs to [WWWROOT]/${DST_FOLDER_NAME})"; exit 1; }
[[ ! -d ${REPO_ROOT} ]] && { echo "Working dir '${REPO_ROOT}' does not exist (somehow)!"; exit 1; }
[[ ! -d "${REPO_ROOT}/library" ]] && { echo "Library folder '${REPO_ROOT}/library' does not exist!"; exit 1; }
[[ ! -d "${REPO_ROOT}/sample" ]] && { echo "Samples folder '${REPO_ROOT}/sample' does not exist!"; exit 1; }

mkdir -p ${DST_FOLDER_PATH}
rm -f ${DST_FOLDER_PATH}/module-list.txt
for n in {library,sample}/*.blah; do echo "${DST_FOLDER_NAME}/$n" >> "${DST_FOLDER_PATH}/module-list.txt"; done
yes | cp -r "${REPO_ROOT}"/{library,sample} "${DST_FOLDER_PATH}"