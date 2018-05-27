#!/bin/sh

PROJECT_NAME="stopping-time"

set -e

CUR_DIR="$(pwd)"
cd ../server
stack build
stack exec js-gen -- >"${CUR_DIR}/src/${PROJECT_NAME}/api.js"
cd "${CUR_DIR}"